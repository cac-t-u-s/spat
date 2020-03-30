
(in-package :om)


(defclass! spat-dsp (spat-object data-stream)
  ((audio-in :accessor audio-in :initform nil :initarg :audio-in :documentation "audio source")
   (dsp-type :initarg :dsp-type :accessor dsp-type :initform "spat5.filterdesign")
   (controls :initarg :controls :accessor controls :initform nil :documentation "list of timed OSC-bundles"))
  (:default-initargs :default-frame-type 'osc-bundle :action 'render-audio))

;;; controls is a list of controller values : 
;;; ("/osc_identifier" value-s)
;;; or automation wih name "/osc_identifier"

(defmethod SpatControllerComponent-name ((self spat-dsp)) 
  (dsp-type self))

(defmethod SpatDSPComponent-name ((self spat-dsp))
  (spat-dsp-component-name (dsp-type self)))
 
;(spat::omspatsetverbose t)
;(spat::omspatisverbose)

(defun spat-dsp-component-name (dsp-compo-name)
  (cadr (find dsp-compo-name
              '(("spat5.filterdesign" "spat5.cascade~") 
                ("spat5.equalizer" "spat5.cascade~")   
                ("spat5.ircamverb" "spat5.ircamverb~")
                ("spat5.routing" "spat5.routing~")
                ("spat5.compressor" "spat5.compressor~")
                ("spat5.graphiceq" "spat5.graphiceq~")
                ("spat5.hlshelf" "spat5.hlshelf~")
                ("spat5.oper" "spat.spat~")
                )
              :key 'car :test 'string-equal)))


(defmethod ensure-init-state ((self spat-dsp))
  (unless (and (controls self) (= 0 (date (car (controls self)))))
    (let ((init (get-gui-state self)))
      (when init
        (setf (controls self) (cons init (controls self)))))))

(defmethod om-init-instance ((self spat-dsp) &optional args)
  (let ((dsp (call-next-method)))
    (ensure-init-state dsp)
    dsp))

;;; TIME-SEQUENCE API
(defmethod time-sequence-get-timed-item-list ((self spat-dsp)) (controls self))
(defmethod time-sequence-set-timed-item-list ((self spat-dsp) list) (setf (controls self) list))

(defmethod time-sequence-make-timed-item-at ((self spat-dsp) at)
  (let ((previous (find at (time-sequence-get-timed-item-list self) :key 'date :test '> :from-end t))) 
    (make-instance 'osc-bundle 
                   :date at
                   :messages (and previous (om-copy (messages previous))))
    ))

(defun same-kind (b1 b2)
  (let ((top1 (cdr (find "/topology" (messages b1) :key 'car :test 'string-equal)))
        (top2 (cdr (find "/topology" (messages b2) :key 'car :test 'string-equal)))
        (resp1 (cdr (find "/response" (messages b1) :key 'car :test 'string-equal)))
        (resp2 (cdr (find "/response" (messages b2) :key 'car :test 'string-equal))))
    (and (equal top1 top2) 
         (equal resp1 resp2))))
   
     
(defun interpolate-messages (b1 b2 time)
  (remove 
   nil 
   (loop for m1 in (messages b1) 
         collect
         (let ((m2 (find (car m1) (messages b2) :key 'car :test 'string-equal)))  
           (cond (;;; case: bool message (T/NIL)
                  (not (numberp (cadr m1))) 
                  (om-copy m1))
                 (;; case: special numeric values (can not be interpolated)
                  (find (car m1) '("/topology" "/response") :test 'string-equal) 
                  (om-copy m1))
                 (;;; case: no next message
                  (null m2) 
                  (om-copy m1))
                 (t ;;; case: otherwise
                  (cons 
                     (car m1)
                     (loop for v1 in (cdr m1)
                           for v2 in (cdr m2) collect
                           (let ((fact (/ (- time (date b1)) (- (date b2) (date b1)))))
                             (cond ((integerp v1) 
                                    (round (+ v1 (* fact (- v2 v1)))))
                                   (t (float (+ v1 (* fact (- v2 v1))))))
                             ))))
                 )))))
              
(defmethod time-sequence-make-interpolated-timed-item-at ((self spat-dsp) time)
  (let ((previous (or (find time (time-sequence-get-timed-item-list self) :key 'date :test '>= :from-end t)
                      (init-state self)))
        (next (find time (time-sequence-get-timed-item-list self) :key 'date :test '<)))      
    (make-instance 
     'osc-bundle 
     :date time
     :messages (if (and next (same-kind previous next))
                   (interpolate-messages previous next time)
                 (om-copy (messages previous))))
    ))
  
; (find 3 '(1 2 3 4 5) :test '< :from-end nil)

;;; DATA-STREAM API
(defmethod data-stream-frames-slot ((self spat-dsp)) 'controls)



(defmethod get-properties-list ((self spat-dsp))
  `((""  
     (:name "Name" :text name)
     (:action "Action" :action action-accessor)
     (:interpol "Interpolation" ,(make-number-or-nil :min 20 :max 1000) interpol)
     (:buffer-size "Buffer size" :number buffer-size-accessor)
     )))


;;;=========================
;;; SYNTH
;;;=========================

;;; messages to set the controller state from recorded OSC stream
;(defmethod spat-set-controller-state-messages ((self spat-dsp) (state osc-bundle))
;  (cond ((string-equal (SpatControllerComponent-name self) "spat5.filterdesign")
;         (loop for msg in (remove "/cascades" (messages state) :key 'car :test 'string-equal)
;               collect (cons (string+ "/set" (car msg)) (cdr msg))))
;        (t ;(print (format nil "unknow spat controller: '~A'" (SpatControllerComponent-name self)))
;         (messages state))
;        ))

(defmethod spat-set-controller-state-messages ((self spat-dsp) (state osc-bundle))
  (loop for msg in (remove "/cascades" (messages state) :key 'car :test 'string-equal)
        collect (cons (string+ "/set" (car msg)) (cdr msg))))


#|
(defmethod spat-object-get-process-messages-at-time ((self spat-dsp) time-ms)
  (let ((b (time-sequence-get-active-timed-item-at self time-ms))) ;; will handle interpolation if needed
    ;; (find time-ms (controls self) :test '<= :key 'date)
    (when b 
      (cond ((string-equal (SpatControllerComponent-name self) "spat5.filterdesign")
             (let ((cascade-message 
                    (or (find "/cascades" (messages b) :key 'car :test 'string-equal)
                        (let ((set-state-messages (spat-set-controller-state-messages self b)))
                          (spat-osc-command (spat-component-ptr (spat-controller self)) set-state-messages)
                          (find "/cascades" (spat-get-state (spat-component-ptr (spat-controller self))) :key 'car :test 'string-equal)))))
               (when cascade-message
                 (list (cons "/channel/*/cascades" (cdr cascade-message)))))
             )
            (t (spat-osc-command (spat-component-ptr (spat-controller self)) (messages b))
               (messages b)
               nil)
            ))))
|#

(defmethod spat-object-get-process-messages-at-time ((self spat-dsp) time-ms)
  (let ((b (time-sequence-get-active-timed-item-at self time-ms))) ;; will handle interpolation if needed
    (when b 
      (let ((set-state-messages (spat-set-controller-state-messages self b)))
        (if (spat-component-window (spat-controller self))
          (spat-osc-command-in-view 
           (spat-component-window (spat-controller self))
           (spat-component-ptr (spat-controller self)) 
           set-state-messages)
          (spat-osc-command 
           (spat-component-ptr (spat-controller self)) 
           set-state-messages))
        (spat-get-dsp-commands (spat-component-ptr (spat-controller self)))))))
       

;;;=========================
;;; PLAY
;;;=========================

(defmethod spat-object-actions ((self spat-dsp) interval)
  (when (action self)
    (sort
     (if (number-? (interpol self))
         (let* ((root (get-active-interpol-time self (car interval))))
           (loop for interpolated-time in (arithm-ser root (1- (cadr interval)) (number-number (interpol self))) 
                 collect (list 
                          interpolated-time 
                          #'(lambda (pt) (funcall (action-fun self) pt)) 
                          (list (time-sequence-make-interpolated-timed-item-at self interpolated-time)))
                 ))
       (loop for b in (filter-list (controls self) (car interval) (cadr interval) :key 'date)
             collect (list (date b)
                           #'(lambda (b) (funcall (action-fun self) b))
                           (list b)  ;; (traj-id point)
                           ))
       ) '< :key 'car)))

;;;=========================
;;; EDITOR
;;;=========================

(defclass spat-dsp-editor (spat-editor) ())

(defmethod object-has-editor ((self spat-dsp)) t)
(defmethod get-editor-class ((self spat-dsp)) 'spat-dsp-editor)

;(defmethod SpatComponent-name ((self spat-dsp-editor)) 
;  (spat-gui-component-name (dsp-type (object-value self))))

(defmethod get-info-command ((self spat-dsp-editor)) 
  #'(lambda () (show-inspector nil nil)))

;;; called by the previous / next buttons
(defmethod get-all-sorted-times ((self spat-dsp))
  (time-sequence-get-internal-times self))

(defmethod build-transport-view ((editor spat-dsp-editor))
  (let ((button-size (omp 17 17)))
    (om-make-layout 'om-row-layout 
                    :subviews (list 
                               (make-time-monitor (timeline-editor editor) :time 0) 
                               nil 
                               (make-play-button editor :size button-size) 
                               (make-pause-button editor :size button-size) 
                               (make-stop-button editor :size button-size)
                               (make-previous-button editor :size button-size) 
                               (make-next-button editor :size button-size)
                               nil
                               (om-make-di 'om-check-box 
                                           :text "dynamic filter edit" :size (omp 150 24) 
                                           :font (om-def-font :font1)
                                           :checked-p (editor-get-edit-param editor :dynamic-edit)
                                           :di-action #'(lambda (item) 
                                                          (editor-set-edit-param editor :dynamic-edit (om-checked-p item))))
                               ))))

;; copied from data-stream-editor
(defmethod editor-delete-contents-from-timeline ((self spat-dsp-editor) timeline-id sel)
  (let ((dsp (object-value self)))
    (mapcar #'(lambda (point) (remove-timed-point-from-time-sequence dsp point)) sel)
    (ensure-init-state dsp)
    (time-sequence-update-internal-times dsp)
    (editor-invalidate-views self)
    (report-modifications self)))

;(defmethod spat-init-messages ((editor spat-dsp-editor)) 
;  (append (call-next-method) '(("/cursor/visible" 1))))

;;; called when something happens in the spat view
(defmethod spat-callback-to-front-editor ((editor spat-dsp-editor) messages)
  (let ((keypressed (find "/keypressed" messages :key 'car :test 'string-equal)))
    (if keypressed
        (let ((key (caddr keypressed))
              (modifier (cadr keypressed)))
          (editor-key-action editor (code-char key)))
      (let* ((obj (object-value editor))
             (spatguicomponent (spat-component-ptr (spat-view-handler (spat-view editor))))
             (current-state (spat-get-state spatguicomponent))
             (date (or (get-cursor-time (timeline-editor editor)) 0)))
        (let ((osc-b (if (editor-get-edit-param editor :dynamic-edit)
                         (find date (time-sequence-get-timed-item-list obj) :key 'item-get-time :test '=)
                       (find date (time-sequence-get-timed-item-list obj) :key 'item-get-time :test '>= :from-end t))))
          (unless osc-b
            (setf osc-b (make-instance 'osc-bundle :date date))
            (insert-timed-point-in-time-sequence obj osc-b)
            (om-invalidate-view (get-g-component (timeline-editor editor) :main-panel)))
          (setf (messages osc-b) current-state)
          (report-modifications editor)
          ))
      )))

(defmethod update-spat-display ((self spat-dsp-editor)) 
  (when (window self) 
    (unless (and (eq (player-get-object-state (player self) (get-obj-to-play self)) :play)
                 (equal (action (object-value self)) 'render-audio))
      ;; in this case the DSP process will not update the spat controller...
      (let* ((dsp (object-value self))
             (spatview (spat-view self))
             (spatviewhandler (and spatview (spat-view-handler spatview))))
        (when (and dsp spatviewhandler)
          (let ((osc-b (or (time-sequence-get-active-timed-item-at 
                            dsp (or (get-cursor-time (timeline-editor self)) 0))
                           (init-state dsp))))
            (when osc-b 
              (spat-osc-command-in-view 
               spatview 
               (spat-component-ptr spatviewhandler) 
               (spat-set-controller-state-messages dsp osc-b)
               )
              ))
          )))
    (call-next-method) ;;; will invalidate the timeline(s)
    ))
      







