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
; spat-editor : a spat controleur + time control
; @author: J. Bresson, J. Garcia
;================================================

(in-package :om)

;;======= spat GUI view =========
;;directly controlled by the spat object using a pointer to the view.

(defclass spat-view (om-view) 
  ((spat-GUI-component :accessor spat-GUI-component :initform nil)
   (spat-object :accessor spat-object :initform nil)
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
  (when (spat-GUI-component self) 
    (spat::OmSpatSetWindowSize (spat-GUI-component self) (w self) (h self))))


;;======= an editor of SPAT-OBJECT containing a spat-view =========

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
  (setf (timeline-editor self) (make-instance 'timeline-editor :object (object self) :container-editor self))
  (spat-init-editor self))


(defmethod spat-init-editor ((self spat-editor)) nil)

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


(defmethod spat-view-init ((self spat-editor))
  (spat-editor-set-spat-component self))


(defmethod init-editor-window ((editor spat-editor))
  (call-next-method)
  (spat-view-init editor)
  (init-messages-to-spat-viewer editor)
  (enable-play-controls editor t)
  (update-source-picts editor)
  (update-spat-display editor)
  (update-timeline-display editor)
  (activate-spat-callback editor)
  t)


(defmethod update-timeline-display ((self spat-editor)) 
  (editor-invalidate-views (timeline-editor self)))

;;;=========================================================

(defmethod update-spat-display ((self spat-editor)) nil)

(defmethod editor-invalidate-views ((self spat-editor))
  (update-timeline-display self)
  (when (spat-view self) (update-spat-display self)))

;;;=========================================================

;;; add stuff for spat-scene (and other sub-classes)
(defmethod spat-update-to-editor ((editor spat-editor) from) nil)

(defmethod update-to-editor ((editor spat-editor) (from t)) 
  (call-next-method)
  (time-sequence-update-internal-times (object-value editor))
  (enable-play-controls editor t)
  (spat-update-to-editor editor from)
  (when (window editor) (editor-invalidate-views editor)))

(defmethod update-to-editor ((editor spat-editor) (from OMBox)) 
  (when (and (window editor) (spat-view editor)) 
    (if (equal (spat-object (spat-view editor)) (object-value editor))
        (call-next-method)
      ;;; it's a new object => reinitialize the spat-view
      (om-close-window (window editor)))))

;;; => reactivate spat-callback ??
(defmethod update-to-editor ((editor spat-editor) (from collection-editor))
  (unless (equal (spat-object (spat-view editor)) (object-value editor))
    (spat-editor-remove-spat-component editor)
    (spat-editor-set-spat-component editor))
  (call-next-method))

;;;=========================================================

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

(defmethod spat-editor-set-spat-component ((editor spat-editor))
  
  (let ((spat-object (object-value editor))
        (view (spat-view editor)))
   
      ;;; not used...
    (setf (id view) (read-from-string (subseq (string (gensym)) 1)))
    
    (let ((ctrl-comp-name (SpatControllerComponent-name spat-object)))
      
      (when ctrl-comp-name
      
        (if (spat::omspatisvalidcomponenttype ctrl-comp-name)
            
            (let ((comp (spat::OmSpatCreateComponentWithType ctrl-comp-name)))

              (om-print-dbg "Create GUI-Component ~A [~A] in ~A" (list comp (remove #\~ ctrl-comp-name) editor) "OM-SPAT")
              (setf (spat-GUI-component view) comp)
                    
              (capi:execute-with-interface 
               (window editor)
               'spat::OmSpatInstallComponentInNSView 
               (spat-GUI-component view) 
               (spat::spat-get-view-pointer view))

              ;(spat::spat-component-register-callback (spat-GUI-component view))
              (setf (spat-object view) spat-object)
              )
          
          (om-beep-msg "OM-SPAT: Wrong GUI component: ~A" ctrl-comp-name))
        
        ))
    
    view))

;;; sometimes we want to do this later (to avoid receive all initialization callbacks...)
(defmethod activate-spat-callback ((editor spat-editor))
  (when (spat-view editor)
    (spat::spat-component-register-callback (spat-GUI-component (spat-view editor)))))

(defmethod spat-editor-remove-spat-component ((editor spat-editor))
  
  (when (and (window editor) 
             (spat-view editor)
             (spat-GUI-component (spat-view editor)))
    
    (capi:execute-with-interface (window editor)
                                 'spat::OmSpatRemoveFromNSView 
                                 (spat-GUI-component (spat-view editor)))
    
    (om-print-dbg "Free GUI GUI-Component ~A in ~A" (list (spat-GUI-component (spat-view editor)) editor) "OM-SPAT")
    (spat::OmSpatFreeComponent (spat-GUI-component (spat-view editor)))
    
    (setf (spat-GUI-component (spat-view editor)) nil)
    (setf (spat-object (spat-view editor)) nil)
    ))


(defmethod editor-close ((self spat-editor))
  (spat-editor-remove-spat-component self)
  (call-next-method))

  
(defmethod spat-callback-to-front-editor ((editor t) bundle-ptr) nil)

(defmethod spat-callback-to-front-editor ((editor collection-editor) bundle-ptr) 
  (spat-callback-to-front-editor (internal-editor editor) bundle-ptr))


(om-with-redefinitions ;; a default version is defined in the om-spat API

  (defun spat::spat-component-handle-callback (component-ptr bundle-ptr)
  
    (declare (ignore component-ptr))

    (let ((frontwin (om-front-window))) ;;; mmpf..
      (unwind-protect
          (let ((messages (decode-bundle-s-pointer-data bundle-ptr)))
            (om-print-dbg 
             "Received from SPAT: ~{~%                         => ~A ~}" 
             (list messages) "OM-SPAT")
            (spat-callback-to-front-editor (editor frontwin) messages)
            )
        (odot::osc_bundle_s_deepFree bundle-ptr))
      ))
  )

(defmethod spat-object-init-GUI-messages ((editor spat-editor)) 
  (messages (car (controls (object-value editor)))))
            
(defmethod init-messages-to-spat-viewer ((editor spat-editor))
  (when (and (spat-view editor) (spat-GUI-component (spat-view editor)))
    (spat-osc-command 
     (spat-GUI-component (spat-view editor))
     (append-set-to-state-messages (spat-object-init-GUI-messages editor))
     (spat-view editor))
    ))

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
; attempt to add a progress bar

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