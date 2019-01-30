;============================================================================
; OM-spat5
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================

;================================================
; SPAT-SCENE : a polyphonic spat controleur
; @author: J. Bresson, J. Garcia
;================================================

(in-package :om)


(defclass! spat-scene (spat-object time-sequence)
  ((audio-in :accessor audio-in :initform nil :initarg :audio-in :documentation "audio source file(s)") ;;; repeated slot to make it appear on the box
   (trajectories :accessor trajectories :initform nil :initarg :trajectories) ;list of 3DC
   (speakers :accessor speakers :initform '((-1 1 0) (1 1 0)) :initarg :speakers)    ;  (1 -1 0) (-1 -1 0)
   (controls :initarg :controls :accessor controls :initform nil :documentation "list of timed OSC-bundles")
   (panning-type :accessor panning-type :initform "angular")
   (reverb :accessor reverb :initform nil)
   ))

(defmethod get-properties-list ((self spat-scene))
  `(("" 
     (:name "Name" :text name)
     (:action "Action" :action action-accessor)
     (:interpol "Interpolation" ,(make-number-or-nil :min 20 :max 1000) interpol)
     (:panning "Panning type" ("angular" "binaural" "vbap3d" "hoa3d") panning-type-accessor "angular")
     (:reverb "Reverb" :bool reverb-accessor nil)
     (:buffer-size "Buffer size" :number buffer-size-accessor))))

(defmethod panning-type-accessor ((self spat-scene) &optional (value nil value-supplied-p))
  (if value-supplied-p
      (progn (setf (panning-type self) value)
        (spat-object-set-audio-dsp self))
    (panning-type self)))

(defmethod reverb-accessor ((self spat-scene) &optional (value nil value-supplied-p))
  (when value-supplied-p
    (setf (reverb self) value)
    (spat-object-set-audio-dsp self)
    (spat-object-set-spat-controller self))
  (reverb self))


(defmethod additional-class-attributes ((self spat-scene)) 
  (append (call-next-method)
          '(action panning-type reverb buffer-size interpol)))


;;; we have to do this because :panning-type is not a class initarg
;(defmethod om-init-instance ((self spat-scene) &optional args)
;  (let ((ss (call-next-method)))
;    
;    (when (and (find-value-in-kv-list args :buffer-size)
;               (not (= (find-value-in-kv-list args :buffer-size) 
;                       (buffer-size self))))
;     (setf (buffer-size ss) (find-value-in-kv-list args :buffer-size))
;      (spat-object-set-audio-dsp self))
;    
;    ss))


(defmethod class-attributes-menus ((self spat-scene)) 
  '((panning-type (("angular" "angular") 
                   ("binaural" "binaural")
                   ("vbap3d" "vbap3d")
                   ("hoa3d" "hoa3d")))))

(defmethod SpatDSPComponent-name ((self spat-scene)) 
  (if (reverb self) "spat5.spat~" "spat5.pan~"))

(defmethod SpatControllerComponent-name ((self spat-scene)) 
  (if (reverb self) "spat5.oper" "spat5.viewer"))

(defmethod n-channels-in ((self spat-scene)) 
  (length (list! (audio-in self))))

(defmethod n-channels-out ((self spat-scene)) 
  (if (string-equal (panning-type self) "binaural")
      2 
    (length (speakers self))))

(defmethod get-obj-dur ((self spat-scene)) 
  (reduce 'max 
          (cons (get-last-time-point self)
                (remove nil (mapcar 'get-obj-dur (audio-in self))))))


(defmethod spat-object-init-DSP-messages ((self spat-scene)) 
  (append (call-next-method)
          `(("/panning/type" ,(panning-type self))
            ("/speakers/xyz" ,.(apply 'append (speakers self))))))
          


;;;======================================================
;;; INIT
;;;======================================================

(defvar *excluded-spat-state-messages* 
  '("/source/number" 
    "/speaker/number" 
    "/format" 
    "/layout"
    "/source/.*/aed"
    "/source/.*/xyz"
    "/source/.*/color"
    "/source/.*/name"
    "/speaker/.*/aed"
    "/speaker/.*/xyz"
    "/speakers/xyz"
    "/speakers/aed"
    ))

(defmethod get-controller-state ((self spat-scene))
  (when (spat-controller self)
    (make-instance 
     'osc-bundle :date 0
     :messages (filter-osc-messages (spat-get-state (spat-controller self))
                                    *excluded-spat-state-messages*))
    ))



(defmethod ensure-init-state ((self spat-scene))
  (spat-osc-command 
   (spat-controller self)
   (append 
    `(("/set/source/number" ,(length (audio-in self)))
      ("/set/speaker/number" ,(length (speakers self)))
      ("/set/format" "xyz"))
    (loop for spk in (speakers self) for n = 1 then (1+ n) append
          (list (cons (format nil "/set/speaker/~D/xyz" n) spk)
                (list (format nil "/set/speaker/~D/editable" n) 0))
          )
    ))
  (call-next-method))
  

;; do this with om-init-instance ??
;(defmethod initialize-instance :after ((self spat-scene) &rest args)
(defmethod om-init-instance ((self spat-scene) &optional initargs)
  
  (call-next-method)

  (when initargs 
  
    (when (find :speakers initargs :key 'car) 
      (setf (speakers self) (copy-list (cadr (find :speakers initargs :key 'car)))))

    (setf (audio-in self) (list! (audio-in self))
          (trajectories self) (list! (trajectories self)))
  
    (let* ((max-len (max (length (list! (trajectories self))) (length (list! (audio-in self))))))

      (loop for i from 0 to (1- max-len) 
            collect (if (nth i (audio-in self)) 
                        (get-sound (nth i (audio-in self)))
                      (om-init-instance (make-instance 'sound)))
            into sounds
            collect (format-traj-as-3DC (nth i (trajectories self))) 
            into trajects
            finally (setf (audio-in self) sounds
                          (trajectories self) trajects))
                            
      (loop for sr in (audio-in self)
            for tr in (trajectories self) do
            (let ((dur (if sr (sound-dur-ms sr) 0)))
              (when (< (get-obj-dur tr) dur)
                (time-sequence-insert-timed-item tr (time-sequence-make-timed-item-at tr dur))))) 

      ;; will be called by om-init-instance (?)
      ;; (time-sequence-update-internal-times self)
      )
    )
  self)

(defmethod format-traj-as-3DC ((self list))
  (let* ((tranformed-lists (mat-trans self))
        (obj (make-instance '3DC 
                   :x-points (nth 0 tranformed-lists) 
                   :y-points (nth 1 tranformed-lists) 
                   :z-points (nth 2 tranformed-lists) 
                   :times (nth 3 tranformed-lists))))
    (om-init-instance obj)))

; new version used for the range functions
(defmethod time-sequence-get-timed-item-list ((self spat-scene))
  (loop for traj in (list! (trajectories self)) 
        when traj append (time-sequence-get-timed-item-list traj)))


(defmethod format-traj-as-3DC ((self 3DC)) (clone self))
(defmethod format-traj-as-3DC ((self bpf)) (objFromObjs self (make-instance '3DC)))

(defmethod get-all-traj-points ((self spat-scene))
  (loop for traj in (trajectories self) append (point-pairs traj)))
  
(defmethod get-all-sorted-times ((self spat-scene))
  (sort 
   (loop for traj in (trajectories self) 
         append (time-sequence-get-internal-times traj))
   '<))

(defmethod spat-scene-min-time ((self spat-scene))
  (if (trajectories self)
      (car (get-all-sorted-times self))
    0))

(defmethod get-last-time-point ((self spat-scene))
  (or (car (last (get-all-sorted-times self))) 0))

(defmethod update-interpol-settings-for-trajs ((self spat-scene))
  (loop for 3dc in (trajectories self) do 
        (setf (interpol 3dc) (interpol self))))
  

(defmethod time-sequence-update-internal-times ((self spat-scene) 
                                                &optional (interpol-mode :constant-speed) 
                                                (duration 10000) (modif-time nil))
  (with-schedulable-object 
   self
   (loop for traj in (trajectories self) do
         (time-sequence-update-internal-times traj))))

;;;=========================================
;;; TIME MARKERS METHODS
;;;=========================================

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-time-markers ((self spat-scene))
  "returns a list of time markers"
  (flat (loop for traj in (trajectories self) 
        collect (get-all-master-points-times traj))))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-elements-for-marker ((self spat-scene) marker)
  "returns a list of elements matching the marker"
  (loop for traj in (trajectories self) 
        collect
        (list traj (point-exists-at-time traj marker))))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod translate-elements-from-time-marker ((self spat-scene) elems dt)
  "translates elements from a time marker with dt"
  (loop for elem in elems
        do
        (when (not (member nil (cdr elem)))
          (temporal-translate-points (car elem) (cdr elem) dt))))

;;;====================================
;;; MINIVIEW
;;;====================================

(defmethod display-modes-for-object ((self spat-scene))
  '(:hidden :text :mini-view))

;;; to be redefined by objects if they have a specific draw/cache strategy
(defmethod get-cache-display-for-draw ((self spat-scene)) 
  (list 
   ;;; just record a zoom factor for drawing
   (loop for p in (append (get-all-traj-points self) (speakers self))
         maximize (if p (sqrt (+ (* (car p) (car p)) (* (cadr p) (cadr p)))) 0))))
   


;;; showing all the traj as lists of x y and z
(defun draw-spat-scene-curves (self box x y w h)
  (declare (ignore box y))
  (let ((num-traj (length (trajectories self)))
        (x-col (om-def-color :red))
        (y-col (om-def-color :green))
        (z-col (om-def-color :blue))
        (min-t (get-first-time self))
        (max-t (get-obj-dur self)))
    (when (> num-traj 0)
      (loop for traj in (trajectories self)
            for i from 0 to (1- num-traj)
            do
            (let* ((ranges (nice-bpf-range traj))
                   (times (time-sequence-get-internal-times traj))
                   (x-t-list (mat-trans (list times (x-points traj))))
                   (x-t-ranges (list min-t max-t (car ranges) (cadr ranges)))
                   (y-t-list (mat-trans (list times (y-points traj))))
                   (y-t-ranges (list min-t max-t (caddr ranges) (cadddr ranges)))
                   (z-t-list (mat-trans (list times (z-points traj))))
                   (z-t-ranges (list min-t max-t (nth 5 ranges) (nth 6 ranges)))
                   (h-new (/ h num-traj))
                   (y-new (* i h-new)))
              ;draw x = f(t)
              (om-draw-line x y-new (+ x w) y-new)
              (draw-bpf-points-in-rect x-t-list
                                       x-col
                                       x-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                                       x (+ y-new 5) w (- h-new 5)
                                       )
              ;draw y = f(t)
              (draw-bpf-points-in-rect y-t-list
                                       y-col
                                       y-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                                       x (+ y-new 5) w (- h-new 5)
                                       )
              ;draw z = f(t)
              (draw-bpf-points-in-rect z-t-list
                                       z-col
                                       z-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                                       x (+ y-new 5) w (- h-new 5)
                                       ) 
              t)))))


(defvar *spat-mini-view-source-radius* 3)   

(defmethod draw-spat-scene ((self spat-scene) x y w h max at)
  (flet ((relpos (coord ref) (+ ref (* coord (/ (- ref 15) max)))))
    (let ((x0 (/ w 2))
          (y0 (/ h 2)))
      (om-with-fg-color (om-def-color :gray)
        (om-with-line '(2 2)
          (om-draw-ellipse (+ x x0) (+ y y0) (- x0 5) (- y0 10))
          (om-draw-line (+ x x0) (+ y y0 -10) (+ x x0) (+ y y0 10))
          (om-draw-line (+ x x0 -10) (+ y y0) (+ x x0 10) (+ y y0)))
        (loop for spk in (speakers self) do
              (when spk (om-draw-rect (+ x (- (relpos (car spk) x0) 4))
                                      (+ y (- (relpos (- (cadr spk)) y0) 4))
                                      8 8 :fill t)))
        )
     
      (loop for traj in (trajectories self) do
            (when traj 
              (let* ((start (car (list! at)))
                     (end (cadr (list! at))) 
                     (pt (get-point-at-time traj start)))
                (when pt
                  (om-with-fg-color (color traj)
                    (om-draw-circle (+ x (relpos (om-point-x pt) x0)) ; - *spat-mini-view-source-radius*)
                                    (+ y (relpos (- (om-point-y pt)) y0)) ; - *spat-mini-view-source-radius*)
                                    (/ h 30) ;(* 2 *spat-mini-view-source-radius*) 
                                    :fill t)
                    ))
                (when end
                  (loop for atp from start to end by 50 do 
                        (let ((pt1 (get-point-at-time traj atp))
                              (pt2 (get-point-at-time traj (+ atp 50))))
                          (when pt1
                            (om-with-fg-color (color traj)
                              (om-draw-circle (+ x (relpos (om-point-x pt1) x0)) ; - *spat-mini-view-source-radius*)
                                              (+ y (relpos (- (om-point-y pt1)) y0)) ; - *spat-mini-view-source-radius*)
                                              2 :fill t)
                              (when pt2
                                (om-draw-line (+ x (relpos (om-point-x pt1) x0)) (+ y (relpos (- (om-point-y pt1)) y0))
                                              (+ x (relpos (om-point-x pt2) x0)) (+ y (relpos (- (om-point-y pt2)) y0)))
                                )
                              )))))
                )))
            )))


   

;;; second version using keyframes
;;need to add the time parameters and it should animate the good keyframe...
(defun draw-spat-scene-frames (self box x y w h frame-w)
  (let* ((dur (get-obj-dur self)) 
         (frames (max 1 (round (/ w frame-w))))
         (frame-width (round (/ w frames))))
    (when (> dur 0)
      (loop for frame from 0 to (1- frames) do
            (let ((frame-time (round (* frame (/ dur frames))))
                  (frame-end-time (round (* (1+ frame) (/ dur frames))))
                  (frame-x (+ x (* frame frame-width))))
              (when (not (equal frame 0))
                (om-draw-line frame-x (+ y 8) frame-x (+ y h -8) :color (om-def-color :dark-gray) :style '(2 2)))
               
              (draw-spat-scene self frame-x y frame-width h 
                              (car (get-display-draw box))
                              (list frame-time frame-end-time)
                              ))))))


(defmethod draw-mini-view ((self spat-scene) (box t) x y w h &optional time)
  (ensure-cache-display-draw box self)
  (let ((time (or time 0))
        (max (car (get-display-draw box))))
    (case (get-edit-param box :view-mode)
      (:3dc (draw-spat-scene-curves self box x y w h))
      (otherwise (draw-spat-scene self x y w h max time)))
    ))
      


(defparameter *min-key-frames-size* 100)

(defmethod draw-maquette-mini-view ((self spat-scene) (box t) x y w h &optional time) 
  (declare (ignore time))
  (ensure-cache-display-draw box self)
  (case (get-edit-param box :view-mode)
    (:3dc (draw-spat-scene-curves self box x y w h))
    (otherwise (draw-spat-scene-frames self box x y w h *min-key-frames-size*)))
  )


;;;===============================================
;;; SYNTH
;;;===============================================

;;; RETURNS THE POSITIONS FRO EACH SOURCE
(defmethod spat-object-get-process-messages-at-time ((self spat-scene) time-ms)
  (remove 
   nil 
   (loop for traj in (trajectories self) 
         for i = 1 then (+ i 1) collect
         (let ((p (time-sequence-get-active-timed-item-at traj time-ms))) ;; will handle interpolation if needed
           ;; (find time-ms (point-list traj) :test '<= :key 'tpoint-internal-time)))
           (when p (list (format nil "/set/source/~D/xyz" i) (om-point-x p) (om-point-y p) (om-point-z p)))))))


; (find 7 '(1 2 3 4 5 6) :test '<=)
; (find 0 '(1 2 3 4 5 6) :test '>= :from-end t)

;;;=============================================
;;; PLAYER
;;;=============================================

(defmethod get-def-action-list ((object spat-scene))
  '(print send-source-as-osc render-audio))

(defmethod arguments-for-action ((fun (eql 'send-source-as-osc)))
  '((:string address "/source/*/xyz")
    (:string host "localhost")
    (:int port 3000)))

(defun send-source-as-osc (src-point &optional (address "/source/*/xyz") (host "localhost") (port 3000))
  (osc-send (list (substitute (elt (number-to-string (car src-point)) 0) #\* address)
                  (3dpoint-x (cadr src-point)) (3dpoint-y (cadr src-point)) (3dpoint-z (cadr src-point)))
            host port))

(defmethod spat-object-actions ((self spat-scene) interval)
  (when (action self)
    (sort 
     (if (number-? (interpol self))
        
         (let* ((root (get-active-interpol-time self (car interval))))
           (loop for interpolated-time in (arithm-ser root (1- (cadr interval)) (number-number (interpol self))) 
                 append (loop for 3dc in (trajectories self) 
                              for i = 1 then (+ i 1) 
                              collect (list 
                                       interpolated-time 
                                       #'(lambda (pt) (funcall (action-fun self) pt)) 
                                       (list (list i (make-default-tpoint-at-time 3dc interpolated-time))) ;; (traj-id point)
                                       ))))
      
       (loop for 3dc in (trajectories self) 
             for i = 1 then (+ i 1) append
             (loop for pt in (filter-list (point-list 3dc) (car interval) (cadr interval) :key 'tpoint-internal-time)
                   collect
                   (list (tpoint-internal-time pt)
                         #'(lambda (ptmp) (funcall (action-fun self) ptmp))
                         (list (list i pt))  ;; (traj-id point)
                         ))) 
       )
     '< :key 'car)))


;;;===============================================
;;; SVG export
;;;===============================================

(defun omcol2svgcolorstr (color)
  (format nil "rgb(~D, ~D, ~D)" 
          (round (* 255 (om-color-r color)))
          (round (* 255 (om-color-g color)))
          (round (* 255 (om-color-b color)))))

;to improve to adapt to the size
(defmethod export-keyframe-as-svg ((self spat-scene) file-path &key  (time 0) (w 300) (h 300) (margins 20) (speakers t))
  :icon 908
  :indoc '("a spat-scene object" "a pathname" "time to draw" "image width" "image height" "margins size" "show speakers" )
  :initvals '(nil nil 0 300 300 20 1)
  :doc "
Exports <self> to SVG format.
"
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :prompt "New SVG file"
                                                       :types '("SVG Files" "*.svg")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let* ((scene (svg::make-svg-toplevel 'svg::svg-1.1-toplevel :height h :width w))
             (center (omp (/ w 2) (/ h 2)))
             (scale (/ (min (/ (- h (* 2 margins)) 2) (/ (- w (* 2 margins)) 2)) 2) ))

        ;draw the frame
        (svg::draw scene 
                   (:rect :x margins :y margins :height (- h (* margins 2)) :width (- w (* margins 2)))
                   :fill "none" :stroke "rgb(0, 0, 0)" :stroke-width 1 )

        ;draw a cross ath the center
        (svg::draw scene 
                   (:line
                    :x1 (- (om-point-x center) (* 0.1 scale)) :y1 (om-point-y center) 
                    :x2  (+ (om-point-x center) (* 0.1 scale)) :y2 (om-point-y center) 
                    :stroke "rgb(0, 0, 0)"))
        (svg::draw scene 
                   (:line
                    :x1 (om-point-x center) :y1 (- (om-point-y center) (* 0.1 scale))
                    :x2 (om-point-x center) :y2 (+ (om-point-y center) (* 0.1 scale))
                    :stroke "rgb(0, 0, 0)"))
        
        ;draw a unit circle
        (svg::draw scene 
                   (:circle :cx (om-point-x center) :cy (om-point-y center) :r (* 1 scale)
                    :stroke "rgb(0, 0, 0)"
                    :fill "none"))
      
        ;draw the speakers
        (when speakers
          (let ((speak_w (* 0.1 scale)))
            (loop for spk in (speakers self) do
                  (when spk
                    (svg::draw scene 
                               (:rect 
                                :x  (- (- (om-point-x center) (* (car spk) scale)) (/ speak_w 2))  
                                :y (- (- (om-point-x center) (* (cadr spk) scale)) (/ speak_w 2)) 
                                :height speak_w
                                :width speak_w)
                               :fill "rgb(0, 0, 0)" :stroke "rgb(0, 0, 0)" :stroke-width 1 )
                    ))))

        ;draw the sources at time
        (loop for traj in (trajectories self)
              for n = 1 then (+ n 1) do
          (when traj 
            (let ((pt (if (number-? (interpol traj))
                          (make-default-tpoint-at-time traj time)
                        (time-sequence-get-active-timed-item-at traj time))))
              (when pt
                (let ((cx (+ (om-point-x center) (* (om-point-x pt) scale)))
                      (cy (-  (om-point-y center) (* scale (om-point-y pt))))
                      (col (omcol2svgcolorstr (or (color traj) (om-def-color :black))))
                      (rad  (* 0.1 scale))
                      (txt (format nil "~D" n )))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill col))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill "none"))
                (svg:text scene (:x (- cx 2) :y (+ cy 3))  (svg:tspan (:font-size "10") txt))
                )))))

        ;draw the time
        (svg:text scene (:x (+ margins 4) :y (+ margins 14))  (format nil "~D" time ) " ms")
  
             (with-open-file (s pathname :direction :output :if-exists :supersede)
               (svg::stream-out s scene)))
        pathname
        )))


;to improve to adapt to the size
(defmethod export-interval-as-svg ((self spat-scene) file-path &key  (start-time 0) (end-time nil) (w 300) (h 300) (margins 20) (speakers t))
  :icon 908
  :indoc '("a spat-scene object" "a pathname" "start-time" "end-time" "image width" "image height" "margins size" "show speakers" )
  :initvals '(nil nil nil nil 300 300 20 1)
  :doc "
Exports <self> to SVG format.
"
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :prompt "New SVG file"
                                                       :types '("SVG Files" "*.svg")))))
    (unless end-time
      (setf end-time (get-obj-dur self)))

    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let* ((scene (svg::make-svg-toplevel 'svg::svg-1.1-toplevel :height h :width w))
             (center (omp (/ w 2) (/ h 2)))
             (scale (/ (min (/ (- h (* 2 margins)) 2) (/ (- w (* 2 margins)) 2)) 2) ))

        ;draw the frame
        (svg::draw scene 
                   (:rect :x margins :y margins :height (- h (* margins 2)) :width (- w (* margins 2)))
                   :fill "none" :stroke "rgb(0, 0, 0)" :stroke-width 1 )

        ;draw a cross ath the center
        (svg::draw scene 
                   (:line
                    :x1 (- (om-point-x center) (* 0.1 scale)) :y1 (om-point-y center) 
                    :x2  (+ (om-point-x center) (* 0.1 scale)) :y2 (om-point-y center) 
                    :stroke "rgb(0, 0, 0)"))
        (svg::draw scene 
                   (:line
                    :x1 (om-point-x center) :y1 (- (om-point-y center) (* 0.1 scale))
                    :x2 (om-point-x center) :y2 (+ (om-point-y center) (* 0.1 scale))
                    :stroke "rgb(0, 0, 0)"))
        
        ;draw a unit circle
        (svg::draw scene 
                   (:circle :cx (om-point-x center) :cy (om-point-y center) :r (* 1 scale)
                    :stroke "rgb(0, 0, 0)"
                    :fill "none"))
      
        ;draw the speakers
        (when speakers
          (let ((speak_w (* 0.1 scale)))
            (loop for spk in (speakers self) do
                  (when spk
                    (svg::draw scene 
                               (:rect 
                                :x  (- (- (om-point-x center) (* (car spk) scale)) (/ speak_w 2))  
                                :y (- (- (om-point-x center) (* (cadr spk) scale)) (/ speak_w 2)) 
                                :height speak_w
                                :width speak_w)
                               :fill "rgb(0, 0, 0)" :stroke "rgb(0, 0, 0)" :stroke-width 1 )
                    ))))

        ;draw the motions at start-time
        (loop for traj in (trajectories self)
              for n = 1 then (+ n 1) do
          (when traj 
            (let ((start-pt (if (number-? (interpol traj))
                          (make-default-tpoint-at-time traj start-time)
                              (time-sequence-get-active-timed-item-at traj start-time)))
                  (end-pt (if (number-? (interpol traj))
                              (make-default-tpoint-at-time traj end-time)
                            (time-sequence-get-active-timed-item-at traj end-time))))

              ;draw a line between start and end
              (when (and start-pt end-pt)
                (let ((spos (find-active-position-at-time traj start-time))
                      (epos (find-active-position-at-time traj end-time)))
                  (loop for idx = spos then (+ idx 1)
                         while (< idx epos) do
                         
                         (let* ((p1 (get-nth-point traj idx))
                               (p2 (get-nth-point traj (1+ idx)))
                               (csx (+ (om-point-x center) (* (om-point-x p1) scale)))
                               (csy (-  (om-point-y center) (* scale (om-point-y p1))))
                               (cex (+ (om-point-x center) (* (om-point-x p2) scale)))
                               (cey (-  (om-point-y center) (* scale (om-point-y p2))))
                               (col (omcol2svgcolorstr (or (color traj) (om-def-color :black)))))
                           (svg::draw scene 
                                      (:line
                                       :x1 csx :y1 csy)
                                      :x2 cex :y2 cey
                                      :stroke col)
                           ))))

              ;draw start point
              (when start-pt
                (let ((cx (+ (om-point-x center) (* (om-point-x start-pt) scale)))
                      (cy (-  (om-point-y center) (* scale (om-point-y start-pt))))
                      (col (omcol2svgcolorstr (or (color traj) (om-def-color :black))))
                      (rad  (* 0.1 scale))
                      (txt (format nil "~D" n )))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke col
                            :fill "none"))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke col
                            :fill "none"))
                (svg:text scene (:x (- cx 2) :y (+ cy 3))  (svg:tspan (:font-size "10") txt))
                ))
              ;draw end-point
              (when end-pt
                (let ((cx (+ (om-point-x center) (* (om-point-x end-pt) scale)))
                      (cy (-  (om-point-y center) (* scale (om-point-y end-pt))))
                      (col (omcol2svgcolorstr (or (color traj) (om-def-color :black))))
                      (rad  (* 0.1 scale))
                      (txt (format nil "~D" n )))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill col))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill "none"))
                (svg:text scene (:x (- cx 2) :y (+ cy 3))  (svg:tspan (:font-size "10") txt))
                )))))

        ;draw the time
        (svg:text scene (:x (+ margins 4) :y (+ margins 14))  (format nil "[~D, ~D]" start-time end-time ) " ms")
  
             (with-open-file (s pathname :direction :output :if-exists :supersede)
               (svg::stream-out s scene)))
        pathname
        )))

