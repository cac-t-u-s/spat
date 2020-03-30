
;;;================================================
;;; spat-editor : a spat controleur + time control
;;;================================================

(in-package :om)

(defclass spat-editor (OMEditor play-editor-mixin) 
  ((timeline-editor :accessor timeline-editor :initform nil)
   (spat-view :accessor spat-view :initform nil)
   (source-picts :accessor source-picts :initform nil)))

(defmethod SpatComponent-name ((self spat-editor)) nil)

;; from play-editor-mixin
(defmethod cursor-panes ((self spat-editor)) (cursor-panes (timeline-editor self)))
(defmethod get-obj-to-play ((self spat-editor)) (object-value self))

(defmethod set-time-display ((self spat-editor) time)
  (set-time-display (timeline-editor self) time)
  (call-next-method))

(defmethod init-editor ((self spat-editor))
  (call-next-method)
  (setf (timeline-editor self) (make-instance 'timeline-editor :object (object self) :container-editor self)))

(defmethod make-editor-window-contents ((editor spat-editor))
  (let ((timeline-container (om-make-layout 'om-simple-layout))
        (spat-view (om-make-view 'spat-view  :size (omp 200 200)))
        (attributes-view (make-default-editor-view editor)))
    
    (setf (spat-view editor) spat-view)
    (when (timeline-editor editor)
      (set-g-component (timeline-editor editor) :main-panel timeline-container)
      (make-timeline-view (timeline-editor editor)))
    
    (om-make-layout 
     'om-column-layout
     :ratios '(0.9 0.1)
     :subviews (list (om-make-layout 'om-row-layout
                                     :ratios '(98 1)
                                     :subviews (list (om-make-layout 'om-simple-layout :subviews (list spat-view))
                                                     attributes-view))
                     timeline-container
                     ))
    ))


(defmethod set-spat-view ((self spat-editor))
  (spat-editor-set-spat-component self))

(defmethod init-editor-window ((editor spat-editor))
  (call-next-method)
  (set-spat-view editor)
  (enable-play-controls editor t)
  (update-source-picts editor)
  (init-spat-viewer editor)
  (update-spat-display editor)
  (update-timeline-display editor))

(defmethod update-timeline-display ((self spat-editor)) 
  (editor-invalidate-views (timeline-editor self)))

(defmethod update-spat-display ((self spat-editor)) 
  (when (window self)
    (om-invalidate-view (get-g-component (timeline-editor self) :main-panel))))

(defmethod editor-invalidate-views ((self spat-editor))
  (update-timeline-display self)
  (when (spat-view self) (update-spat-display self)))

(defmethod update-to-editor ((editor spat-editor) (from t)) 
  (call-next-method)
  (when (window editor) (editor-invalidate-views editor)))

(defmethod update-to-editor ((editor spat-editor) (from OMBox)) 
  (call-next-method)
  (when (and (window editor) (spat-view editor)) 
    (let ((spat-controller (spat-view-handler (spat-view editor))))
      (if (and spat-controller 
               (equal (spat-component-object spat-controller)
                      (object-value editor)))
          (call-next-method)
        ;;; new object
        (om-close-window (window editor)))
      )))

(defmethod update-to-editor ((editor spat-editor) (from collection-editor))
  (let ((current-spat-controller-in-view (spat-component-ptr (spat-view-handler (spat-view editor))))
        (current-spat-controller-in-object (spat-component-ptr (spat-controller (object-value editor)))))
        
    (unless (equal current-spat-controller-in-view current-spat-controller-in-object)
      (spat-editor-remove-spat-component editor)
      (spat-editor-set-spat-component editor)
      ))
  (call-next-method))


(defmethod select-all-command ((self spat-editor))
  #'(lambda () 
      (setf (selection (timeline-editor self)) 
            (time-sequence-get-timed-item-list (object-value self)))
      (editor-invalidate-views self)))

(defmethod editor-key-action ((self spat-editor) key)
  (unless (call-next-method)
    (editor-key-action (timeline-editor self) key)))

;;===================
;; SPAT-VIEWER
;;===================

;;======= spat scene view =========
;;directly controlled by the spat object using a pointer to the view.

(defclass spat-view (om-view) 
  ((spat-view-handler :accessor spat-view-handler :initform nil)
   (id :accessor id :initform nil :initarg :id)))

(defmethod get-spat-view-id ((self t)) -1)
(defmethod get-spat-view-id ((self spat-editor)) 
  (if (spat-view self)
      (id (spat-view self))
    -2))

(defun find-window-with-spat-view (id)
  (find id (capi::collect-interfaces 'OMEditorWindow)
        :test #'(lambda (id win) 
                  (= id (get-spat-view-id (editor win))))))


(defmethod om-view-resized ((self spat-view) size) 
  (call-next-method)
  (when (spat-view-handler self) 
    (spat-osc-command 
     (spat-component-ptr (spat-view-handler self)) 
     `(("/om/window/size" ,(w self) ,(h self)))
     self)
    ))



(defmethod spat-editor-set-spat-component ((editor spat-editor))
  (let* ((spat-object (object-value editor))
         (view (spat-view editor))
         (spat-controller (spat-controller spat-object)))
    
    ;;; not used...
    (setf (id view) (read-from-string (subseq (string (gensym)) 1)))
    
    (when (and spat-controller (spat-component-ptr spat-controller))
      (om-print-dbg "integrating spat-controller ~A [~A]" 
                    (list (spat-component-ptr spat-controller) (remove #\~ (spat-component-type spat-controller)))
                    "OM-SPAT EDITOR")
      
      (capi:execute-with-interface 
       (window editor)
       'spat::OmSpatInstallComponentInNSView 
       (spat-component-ptr spat-controller) 
       (spat::spat-get-view-pointer view))

      (setf (spat-view-handler view) spat-controller)
      (spat::spat-component-register-callback (spat-component-ptr spat-controller))
      (setf (spat-component-window spat-controller) (window editor))
      )
    
    view))

(defmethod spat-editor-remove-spat-component ((editor spat-editor))
  
  (when (and (window editor) 
             (spat-view editor)
             (spat-view-handler (spat-view editor)))

  (capi:execute-with-interface 
       (window editor)
       'spat::OmSpatRemoveFromNSView 
       (spat-component-ptr (spat-view-handler (spat-view editor))))

  (setf (spat-component-window (spat-view-handler (spat-view editor))) nil)
  
))

(defmethod editor-close ((self spat-editor))
  (spat-editor-remove-spat-component self)
  (call-next-method))

  
(defmethod spat-callback-to-front-editor ((editor t) bundle-ptr) nil)
(defmethod spat-callback-to-front-editor ((editor collection-editor) bundle-ptr) 
  (spat-callback-to-front-editor (internal-editor editor) bundle-ptr))

(defun spat::spat-component-handle-callback (component-ptr bundle-ptr)
  (let ((frontwin (om-front-window))) ;;; mmpf..
    (unwind-protect
        (spat-callback-to-front-editor (editor frontwin) 
                                       (decode-bundle-s-pointer-data bundle-ptr))
      (odot::osc_bundle_s_deepFree bundle-ptr))))

(defmethod spat-init-messages ((editor spat-editor)) 
  `(("/om/window/size" ,(w (spat-view editor)) ,(h (spat-view editor)))))
            
(defmethod init-spat-viewer ((editor spat-editor))
  (when (and (spat-view editor) (spat-view-handler (spat-view editor)))
    (spat-osc-command 
     (spat-component-ptr (spat-view-handler (spat-view editor)))
     (spat-init-messages editor)
     (spat-view editor))))

;;===================
;; PLAY FUNCTIONS
;; from play-editor-mixin
;;===================

(defmethod get-all-sorted-times ((self spat-object)) nil)

(defmethod editor-next-step ((self spat-editor))
  (let ((next-time (find (get-cursor-time (timeline-editor self))
                         (get-all-sorted-times (object-value self))
                         :test '<)))
    (if next-time
        (set-cursor-time (timeline-editor self) next-time)
    (om-beep))))
    
(defmethod editor-previous-step ((self spat-editor)) 
  (let ((previous-time (find (get-cursor-time (timeline-editor self))
                         (get-all-sorted-times (object-value self))
                         :test '> :from-end t)))
    (if previous-time
        (set-cursor-time (timeline-editor self) previous-time)
      (om-beep))))

(defmethod play-editor-callback ((self spat-editor) time)
  (update-spat-display self)
  (call-next-method))


;;;===============================
;;; SOURCES
;;;===============================

;;; In spat-objects the timeline left item is a source file selection button
(defmethod make-timeline-left-item ((self spat-editor) id)
  (om-make-view 
               'om-view :size (omp 15 15)
               :subviews 
               (list  
                (om-make-graphic-object 
                 'om-icon-button :size (omp 15 15) :position (omp 0 0)
                 :icon 'folder :icon-pushed 'folder-pushed
                 :lock-push nil
                 :action #'(lambda (b)
                             (declare (ignore b))
                             (editor-stop self)
                             (let ((snd (om-init-instance (objFromObjs :choose-file (make-instance 'sound))))
                                   (spat-obj (object-value self)))
                               (when snd 
                                 (if (consp (audio-in spat-obj))
                                     (setf (nth id (audio-in spat-obj)) snd)
                                   (setf (audio-in spat-obj) (list snd)))
                                 (update-source-picts self)
                                 (spat-object-set-audio-dsp spat-obj)
                                 (reinit-ranges (timeline-editor self)))))
                 ))))


(defmethod update-source-picts ((editor spat-editor))
  (setf (source-picts editor)
        (loop for src in (list! (audio-in (object-value editor)))
              collect (when (subtypep (type-of src) 'sound)
                        (let ((*def-sound-color* (om-gray-color 0.7)))
                          (list (get-cache-display-for-draw src)
                                (get-obj-dur src))
                          )))))

;;; from time-sequence
(defmethod draw-timeline-background ((self spat-editor) view id)
  (when (and (car (nth id (source-picts self)))
             (not (equal (car (nth id (source-picts self))) :error)))
    (let* ((pict (car (nth id (source-picts self))))
           (dur (cadr (nth id (source-picts self))))
           (pw (om-pict-width pict))
           (x (* (/ pw dur) (x1 view)))
           (w (* (/ pw dur) (- (x2 view) (x1 view)))))
      (om-draw-picture pict :x 0 :y 0 :w (w view) :h (h view)
                       :src-x x :src-w w))))

#|
;tentative de mettre un prgress bar mais ne se met pas bien à jour...
; Besoin à mon avis car charger les waverform est long...

(capi:define-interface progress-bar-spat ()
  ()
  (:panes
   (progress-bar
    capi:progress-bar
    :start 0
    :end 100
    ))
  (:layouts
   (progress-layout
    capi:switchable-layout
    '(progress-bar)))
  (:default-initargs
   :title "Loading SpatScene"
   :best-width 300
   :layout 'progress-layout))

(let ((progress-bar (make-instance 'capi:progress-bar))
      (steps 20))
  (capi:contain progress-bar)
  (loop  for i from 0 to steps
         do 
         (setf (capi:range-slug-start progress-bar) (* (/ 100 (1- steps)) i))
         (gp::invalidate-rectangle progress-bar) 
         (sleep .2))
  (setf (capi:range-slug-start progress-bar) 100))
|#