;;;====================================
;;; SPAT EDITOR
;;;====================================

(in-package :om)

;;;===============================
;;; Spat-Scene editor
;;;===============================

(defclass spat-scene-editor (spat-editor) 
  ((active-items :accessor active-items :initform nil)
   (source-editors :accessor source-editors :initform nil)
   (3D-view :accessor 3D-view :initform nil)
   ))

(defmethod object-default-edition-params ((self spat-scene))
  `((:view-mode :spat)))

(defmethod object-has-editor ((self spat-scene)) t)
(defmethod get-editor-class ((self spat-scene)) 'spat-scene-editor)

(defmethod init-editor ((self spat-scene-editor))
  (call-next-method)
  (set-cursor-time (timeline-editor self) (spat-scene-min-time (object-value self)))
  (init-3dc-actions self))

(defmethod close-source-editors ((self spat-scene-editor))
  (loop for s-ed in (source-editors self) do 
        (when (window (cadr s-ed)) (om-close-window (window (cadr s-ed)))))
  (setf (source-editors self) nil))

(defmethod init-3dc-actions ((self spat-scene-editor))
  (let ((3dc-list (trajectories (object-value self))))
    (loop for curve in 3dc-list
          for n = 1 then (+ n 1) do
          ; (setf (action curve) 'osc-3dc-to-spat)
          (setf (name curve) (number-to-string n)))))

;;; called from a child-editor when the value is edited
(defmethod update-to-editor ((editor spat-scene-editor) (from t)) 
  (time-sequence-update-internal-times (object-value editor))
  (update-interpol-settings-for-trajs (object-value editor))
  (call-next-method))


;;; called when the box updates its value
(defmethod update-to-editor ((editor spat-scene-editor) (from OMBoxEditCall)) 
  (close-source-editors editor)
  (call-next-method)
  (update-source-picts editor)
  (make-timeline-view (timeline-editor editor))
  (enable-play-controls editor t))

(defmethod editor-invalidate-views ((self spat-scene-editor))
  (when (spat-view self)
    (update-spat-view-sources self))
  (call-next-method))


;;======== Editor Window ========

(defmethod editor-get-all-time-sequences ((self spat-scene-editor)) 
  (trajectories (object-value self)))

(defmethod editor-get-time-sequence ((self spat-scene-editor) id)
  (when id (nth id (trajectories (object-value self)))))

(defmethod build-transport-view ((editor spat-scene-editor))
  (let ((button-size (omp 17 17)))
    (om-make-layout 'om-row-layout 
                    :subviews (list 
                               (make-time-monitor (timeline-editor editor) :time 0) 
                               :separator 
                               (make-play-button editor :size button-size) 
                               (make-pause-button editor :size button-size) 
                               (make-stop-button editor :size button-size)
                               (make-previous-button editor :size button-size) 
                               (make-next-button editor :size button-size) ))))


(defmethod make-editor-window-contents ((editor spat-scene-editor))
  (let ((timeline-container (om-make-layout 'om-simple-layout))
        (control-view (om-make-layout 'om-column-layout)))
    
    (set-g-component editor :control-view control-view)
    
    (om-add-subviews control-view 
                     (om-make-di 'om-button :text "+" 
                                 :size (omp 40 24)
                                 :di-action #'(lambda (b) 
                                                (declare (ignore b))
                                                (add-source editor)))
                                               
                     (om-make-di 'om-button :text "-" 
                                 :size (omp 40 24)
                                 :di-action #'(lambda (b) 
                                                (declare (ignore b))
                                                (remove-source editor)))
                     nil
                     (om-make-di 'om-popup-list :items '(:spat :3dc) 
                                 :value (editor-get-edit-param editor :view-mode)
                                 :size (omp 80 24)
                                 :font (om-def-font :font1)
                                 :di-action #'(lambda (b) 
                                                (case (om-get-selected-item-index b)
                                                  (0 (unless (equal (editor-get-edit-param editor :view-mode) :spat)
                                                       (update-view-mode editor :spat)))
                                                  (1 (unless (equal (editor-get-edit-param editor :view-mode) :3dc)
                                                       (update-view-mode editor :3dc)))
                                                  ))))
    
    (set-g-component (timeline-editor editor) :main-panel timeline-container)
    (make-timeline-view (timeline-editor editor))

    (set-g-component 
     editor 
     :spat-view-container 
     (om-make-layout 'om-simple-layout 
                     :subviews (list 
                                (if (equal (editor-get-edit-param editor :view-mode) :spat)
                                    (setf (spat-view editor) (om-make-view 'spat-view :size (omp 200 200)))
                                  (setf (3d-view editor) (make-spat-scene-3D-view editor)))
                                )))
   
     
    (om-make-layout 
     'om-column-layout
     :ratios '(0.9 0.1)
     :subviews (list 
                (om-make-layout 'om-row-layout
                                :ratios '(98 1 1)
                                :subviews
                                (list (get-g-component editor :spat-view-container) 
                                      control-view 
                                      (make-default-editor-view editor)))
                timeline-container
                ))
    ))

(defmethod editor-window-init-size ((self spat-scene-editor)) (om-make-point 500 600))


;(defmethod SpatComponent-name ((self spat-scene-editor)) "spat.viewer")



  
(defmethod set-spat-view ((self spat-scene-editor))
  (when (spat-view self)
    (call-next-method))
  (when (3D-view self)
    (om-init-3d-view (3D-view self)))
  )

(defmethod update-view-mode ((self spat-scene-editor) mode)
  
  (editor-set-edit-param self :view-mode mode)

  (cond
   ((equal mode :spat) (set-spat-view-mode self))
   ((equal mode :3dc) (set-3D-view-mode self)))
        
  (report-modifications self))



(defun set-spat-view-mode (editor)
  (setf (spat-view editor) (om-make-view 'spat-view :size (omp 200 200)))
  (om-remove-all-subviews (get-g-component editor :spat-view-container))
  (setf (3D-view editor) nil)
  (om-add-subviews 
   (get-g-component editor :spat-view-container)
   (spat-view editor))
  (spat-editor-set-spat-component editor)
  (init-spat-viewer editor)
  (update-spat-display editor))


    
(defclass spat-scene-3D-view (om-opengl-view) ())


(defun make-spat-scene-3D-view (editor)
  (om-make-view 'spat-scene-3D-view
                      :editor editor
                      :bg-color (om-def-color :dark-gray)
                      :use-display-list nil
                      :g-objects (create-GL-objects editor)
                      ))

  
(defun set-3D-view-mode (editor)
  (spat-editor-remove-spat-component editor)
  (om-remove-all-subviews (get-g-component editor :spat-view-container))
  (setf (spat-view editor) nil)
  (setf (3D-view editor) (make-spat-scene-3D-view editor))
  (om-add-subviews 
   (get-g-component editor :spat-view-container)
   (3D-view editor))
  (om-init-3d-view (3D-view editor))
  )

;;; not at all optimized.. (recreates objects all he time, even if they don't change)
(defmethod create-GL-objects ((self spat-scene-editor))
  (let ((ss (object-value self)))
    (append 
     (loop for src in (audio-in ss) 
           for traj in (trajectories ss) 
           when (or (null src) (not (mute src)))
           collect 
           (make-instance 
            '3D-lines 
            :points (format-3d-points traj) :color (color traj) 
            :draw-style :draw-all :line-width 2))
     (mapcar  #'make-3D-background-element 
              (make-bg-speakers (speakers ss)))
     )))

(defmethod update-gl-display ((self spat-scene-editor)) 
  (when (3D-view self)
    (om-set-gl-objects (3D-view self) (create-GL-objects self))
    (om-invalidate-view (3D-view self))
    ))



(defmethod om-draw-contents ((self spat-scene-3D-view))
  (let* ((editor (editor self))
         (ss (object-value editor))
         (time (player-get-object-time (player editor) ss)))
    (when (equal (player-get-object-state (player editor) ss) :play)
      (loop for src in (audio-in ss) 
            for traj in (trajectories ss)
            when (or (null src) (not (mute src))) do
            (let ((point (time-sequence-get-active-timed-item-at traj time)))
              (when point
                (opengl:gl-push-matrix) 
                (opengl:gl-color4-f 0.9 0.3 0.1 1.0)
                (draw-sphere (point-to-list point) .1)
                (opengl:gl-pop-matrix))
              )))))

      
;; (restore-om-gl-colors-and-attributes)



;;;=============================
;;; ADD / REMOVE SOURCES
;;;=============================

(defmethod add-source ((self spat-scene-editor))
  (let* ((ss (object-value self))
         (last-traj-index (1- (length (trajectories ss))))
         (last-col (and (trajectories ss) (color (nth last-traj-index (trajectories ss))))))

    (setf (audio-in ss) (append (audio-in ss) (list nil))) ; (number-to-string (+ last-traj-index 2)))))
    (setf (trajectories ss) 
          (append (trajectories ss) 
                  (list (om-init-instance 
                         (make-instance 
                          '3DC 
                          :x-points 0 :y-points 0 :z-points 0 :times 0
                          :color (and last-col (find-next-color-in-golden-palette last-col)))))))
    (make-timeline-view (timeline-editor self))
    (enable-play-controls self t)
    (init-3dc-actions self)
    (update-source-picts self)
    (spat-object-set-audio-dsp ss)
    (update-spat-display self)
    (report-modifications self)))

(defmethod remove-source ((self spat-scene-editor))
  (when (trajectories (object-value self))
    (let* ((ss (object-value self))
           (current-selection (or (get-selected-timelines (timeline-editor self))
                                  (list (1- (length (trajectories ss)))))) ;;; if no selection : will remove the last source
           )
      (loop for ns in current-selection do
            (let ((ed (find ns (source-editors self) :test '= :key 'car)))
              (when ed
                (when (window (cadr ed)) (om-close-window (window (cadr ed))))
                (setf (source-editors self) (remove ed (source-editors self))))
              (setf (audio-in ss) (remove-nth ns (audio-in ss)))
              (setf (trajectories ss) (remove-nth ns (trajectories ss)))
              ))
      
      (make-timeline-view (timeline-editor self))
      (enable-play-controls self t)
      (set-selected-timelines (timeline-editor self) nil)
      (om-set-layout-ratios (main-view self) '(0.9 0.1))
      (update-spat-display self)
      (update-source-picts self)
      (spat-object-set-audio-dsp ss)
      (report-modifications self)
      )))


(defmethod spat-source-dbclicked ((self spat-scene-editor) n)
  (let ((ed? (find n (source-editors self) :test '= :key 'car)))
    (if ed? (open-editor-window (cadr ed?))
      (let* ((traj (nth n (trajectories (object-value self))))
             (ed (make-instance (get-editor-class traj) 
                                :object (make-instance 
                                         'omabstractcontainer 
                                         :contents traj
                                         :edition-params 
                                         `((:background ,(make-bg-speakers (speakers (object-value self))))))
                                :related-editors (list self))))
        (setf (source-editors self) (cons (list n ed) (source-editors self)))
        (init-editor ed)
        (open-editor-window ed)
        ))))

(defmethod editor-delete-contents-from-timeline ((self spat-scene-editor) timeline-id sel)
  (let ((traj (nth timeline-id (trajectories (object-value self)))))
    (mapcar #'(lambda (point) (remove-timed-point-from-time-sequence traj point)) sel)
    (time-sequence-update-internal-times traj))
  (editor-invalidate-views self)
  (report-modifications self))

;(defmethod open-trajectory-collection ((self spat-scene-editor))
;  (let* ((collection (make-instance 'collection :obj-list (trajectories (object-value self))))
;         (ed (make-instance (get-editor-class collection) 
;                            :object (make-instance 'omabstractcontainer :contents collection)
;                            :related-editors (list self))))
;    (init-editor ed)
;   (open-editor-window ed)))


;;;====================================
;;; EDITOR WINDOW
;;;====================================


(defmethod spat-init-messages ((editor spat-scene-editor)) 
  (append (call-next-method) '(("/layout" "single"))))

;;; UPDATE EVERYTHING FROM THE CURRENT STATE
;;; (not optimal !!)
(defmethod update-spat-display ((self spat-scene-editor)) 
  (when (window self)

    (when (3D-view self)
      (update-gl-display self))
    
    (let* ((ss (object-value self))
           (spatview (spat-view self))
           (spatviewhandler (and spatview (spat-view-handler spatview))))

      (when spatviewhandler
        
        (spat-osc-command  
         (spat-component-ptr spatviewhandler)
         (append 
          (list (list "/source/number" (length (audio-in ss)))
                (list "/speaker/number" (length (speakers ss)))
                (list "/set/format" "xyz"))
          (loop for spk in (speakers ss) for n = 1 then (1+ n) append
                (list (cons (format nil "/set/speaker/~D/xyz" n) spk)
                      (list (format nil "/set/speaker/~D/editable" n) 1))
                ))
         spatview)

        (update-spat-view-sources self)
        
        (loop for traj in (trajectories ss)
              for src in (audio-in ss)
              for n = 1 then (+ n 1) do 
              (let ((col (or (color traj) (om-def-color :green)))
                    (pt (time-sequence-get-active-timed-item-at ;;; here goes the interpolation
                                                                traj 
                                                                (or (get-cursor-time (timeline-editor self)) 0))))
                (spat-osc-command
                 (spat-component-ptr spatviewhandler)
                 (remove 
                  nil 
                  (list (when col (list (format nil "/set/source/~D/color" n) 
                                        (coerce (om-color-r col) 'single-float)
                                        (coerce (om-color-g col) 'single-float) 
                                        (coerce (om-color-b col) 'single-float) 
                                        (if (and src (mute src)) 0.1 1.0)
                                        ))
                        (list (format nil "/set/source/~D/name" n) (format nil "~A" n))
                      (and pt (list (format nil "/set/source/~D/xyz" n) (om-point-x pt) (om-point-y pt) (om-point-z pt)))))
                 spatview)
                )))
      ;; (call-next-method) ;;; will invalidate the timeline(s)
      )))

;; source selection
(defmethod update-spat-view-sources ((self spat-scene-editor))
  (when (window self)
    (let* ((spatview (spat-view self))
           (spatviewhandler (and spatview (spat-view-handler spatview)))
           (ids (get-selected-timelines (timeline-editor self))))
      (when (and spatviewhandler ids)
        
        (spat-osc-command
         (spat-component-ptr spatviewhandler) 
         (loop for n from 0 to (1- (length (audio-in (object-value self)))) append
               (list 
                (list (format nil "/set/source/~D/select" (1+ n)) (if (find n ids) 1 0))
                                 ;(list (format nil "/set/source/~D/visible" (1+ n)) (if (find n (muted-sources self)) 0 1))
                ))
         spatview)

        ))))


;;======================
;;SpatVIEW CALLBACK (UPDATES)
;;========================

(defmethod update-position-at-time ((self time-sequence) new-p time)
  (let ((p (point-exists-at-time self time)))
    (if p (om-point-set-values-from-point p new-p)
      (progn
        (om-point-set new-p :time time) 
        (insert-timed-point-in-time-sequence self new-p)))))

(defun source-moved-callback (editor n pos)
  (let* ((ss (object-value editor))
         (traj (nth n (trajectories ss))))
    (update-position-at-time traj (make-3Dpoint :x (nth 0 pos) :y (nth 1 pos) :z (nth 2 pos))
                             (or (get-cursor-time (timeline-editor editor)) 0))
    ))

(defun source-selected-callback (editor n t-or-nil)
  (when (timeline-editor editor)
    (set-selected-timelines (timeline-editor editor) 
                            (if t-or-nil
                                (cons n (get-selected-timelines (timeline-editor editor)))
                              (remove n (get-selected-timelines (timeline-editor editor)) :test '=))
                            )
    ))

(defun source-dbclick-callback (editor n)
  (spat-source-dbclicked editor n))

(defun speaker-moved-callback (editor n pos)
  (setf (nth n (speakers (object-value editor))) pos)
  (report-modifications editor))


;;;======================
;;; MAIN CALLBACK HANDLER
;;;======================
(defmethod spat-callback-to-front-editor ((editor spat-scene-editor) messages)
  (let ((keypressed (find "/keypressed" messages :key 'car :test 'string-equal)))
    (if keypressed
        (let ((key (caddr keypressed))
              (modifier (cadr keypressed)))
          (editor-key-action editor (code-char key)))
      (loop for mess in messages do 
            (let* ((address-tokens (cdr (string-to-list (car mess) "/")))
                   (obj (car address-tokens)))
            
              (cond ((string-equal obj "source")
                     (let ((n (read-from-string (cadr address-tokens)))
                           (action (caddr address-tokens)))
                       (cond ((string-equal action "select")
                              (let ((val (cadr mess)))
                                (source-selected-callback editor (1- n) val)))
                             ((string-equal action "xyz")
                              (source-moved-callback editor (1- n) (cdr mess)))
                             ((string-equal action "doubleclick")
                              (source-dbclick-callback editor (1- n)))
                             (t nil))
                       ))
                      
                    ((string-equal obj "speaker")
                     (let ((n (read-from-string (cadr address-tokens)))
                           (action (caddr address-tokens)))
                       (cond ((string-equal action "xyz")
                              (speaker-moved-callback editor (1- n) (cdr mess)))
                             (t nil))
                       ))
                        
                    (t ;(print mess) 
                     nil))
                  
              )))
    ))

;;===================
;; TIMELINE
;;===================

(defmethod make-timeline-left-item ((self spat-scene-editor) id)
  (let* ((src (nth id (audio-in (object-value self))))
         (cb (om-make-di  
              'om-check-box :size (omp 15 15) 
              :enable src
              :checked-p (and src (not (mute src)))
              :di-action #'(lambda (b) 
                             (when src (setf (mute src) (not (om-checked-p b))))
                             (update-spat-display self))))
         (folder-b (om-make-graphic-object 
                    'om-icon-button :size (omp 15 15) :position (omp 0 0)
                    :icon 'folder :icon-pushed 'folder-pushed
                    :lock-push nil
                    :action #'(lambda (b)
                                (declare (ignore b))
                                (let ((snd (om-init-instance (objFromObjs :choose-file (make-instance 'sound)))))
                                  (when snd 
                                    (let* ((ss (object-value self))
                                           (traj (nth id (trajectories ss)))
                                           (point (make-default-tpoint-at-time traj (get-obj-dur snd))))
                                      (setf (nth id (audio-in ss)) snd)
                                      (insert-timed-point-in-time-sequence traj point))
                                    (update-source-picts self)
                                    (om-enable-dialog-item cb t)
                                    (om-set-check-box cb t)
                                    (reinit-ranges (timeline-editor self)))))
                    )))
    (om-make-layout 
     'om-row-layout :position (omp 0 0) :size (omp 30 15)
     :subviews (list 
                (om-make-view 
                 'om-view :size (omp 15 15)
                 :subviews (list folder-b))
                cb))
     ))

;;===================
;; KEY 
;;===================

(defmethod editor-key-action ((editor spat-scene-editor) key)
  (case key
    (:om-key-esc 
     (set-selected-timelines (timeline-editor editor) nil))
    (otherwise nil)
    )
  (call-next-method))

(defmethod editor-stop ((self spat-editor))
  (call-next-method)
  (update-spat-display self)
  (update-timeline-display self))

(defmethod editor-reset-interval ((self spat-scene-editor))
  (call-next-method)
  (update-spat-display self)
  (update-timeline-display self))


