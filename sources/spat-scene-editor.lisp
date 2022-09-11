;============================================================================
;   spat library for OM#
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

;;;====================================
;;; SPAT SCENE EDITOR
;;; @author: J. Garcia, J. Bresson
;;;====================================

(in-package :om)

;;;===============================
;;; Spat-Scene editor
;;;===============================

(defclass spat-scene-editor (spat-editor)
  ((active-items :accessor active-items :initform nil)
   (3DC-editor :accessor 3DC-editor :initform nil)
   (selected-sources :accessor selected-sources :initform nil)
   ))

(defmethod object-default-edition-params ((self spat-scene))
  `((:view-mode :spat)))

(defmethod object-has-editor ((self spat-scene)) t)
(defmethod get-editor-class ((self spat-scene)) 'spat-scene-editor)


(defmethod editor-get-all-time-sequences ((self spat-scene-editor))
  (trajectories (object-value self)))

(defmethod editor-get-time-sequence ((self spat-scene-editor) id)
  (when id (nth id (trajectories (object-value self)))))


(defmethod spat-init-editor ((self spat-scene-editor))

  (set-cursor-time (timeline-editor self) (spat-scene-min-time (object-value self)))

  (let* ((main-src (or (car (selected-sources self)) 0))
         (3D-ed (make-instance '3DC-editor
                               :container-editor self
                               :object (make-instance 'OMAbstractContainer
                                                      :contents (nth main-src (trajectories (object-value self)))
                                                      :edition-params
                                                      `((:background ,(make-bg-speakers (speakers (object-value self))))))
                               :timeline-enabled nil
                               :color-options nil
                               :with-default-components nil
                               )
                ))

    (setf (3DC-editor self) 3D-ed)
    (init-editor 3D-ed)
    )
  )







; called from a child-editor (2D-views or timeline-views) when the value is edited
(defmethod spat-update-to-editor ((editor spat-scene-editor) from)

  (let ((selected-timelines (get-selected-timelines (timeline-editor editor))))

    (unless (or (null selected-timelines)
                (equal (selected-sources editor) selected-timelines))
      ;;; select only the first one (for BPC editing etc.)
      (select-source editor (car selected-timelines)))

    (case (editor-get-edit-param editor :view-mode)

      (:3DC
       (let ((3Ded (3DC-editor editor)))
         (unless (equal from 3Ded)
           (setf (selection 3Ded) (get-indices-from-points (object-value 3Ded) (selection (timeline-editor editor))))
           (update-default-view 3Ded)
           (update-sub-editors 3Ded)
       ;(update-to-editor 3Ded editor)
           )))

      (:spat
       ;;; update the viewer if switch from pan to spat
       (when (and (spat-view editor)
                  (spat-gui-component (spat-view editor))
                  (not (string-equal (spat::omspatgetcomponenttype (spat-gui-component (spat-view editor)))
                                     (if (reverb (object-value editor)) "spat5.oper" "spat5.viewer"))))

         (spat-editor-remove-spat-component editor)
         (spat-editor-set-spat-component editor)
         (init-messages-to-spat-viewer editor)
         (update-display-contents editor)
         (activate-spat-callback editor))
       ))
    ))

(defmethod editor-invalidate-views ((self spat-scene-editor))
  (call-next-method)
  (when (3dc-editor self)
    (editor-invalidate-views (3dc-editor self))))


(defmethod update-to-editor ((editor spat-scene-editor) (from 3dc-editor))
  (call-next-method)
  (update-to-editor (timeline-editor editor) from))

(defmethod update-to-editor ((editor spat-scene-editor) (from timeline-editor))
  (call-next-method)
  (update-to-editor (3dc-editor editor) from))


(defmethod editor-close :before ((editor spat-scene-editor))
  (editor-close (3dc-editor editor)))


;;=========================
;; WINDOW CONTENTS
;;=========================

(defmethod editor-window-init-size ((self spat-scene-editor)) (om-make-point 500 600))

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
                               (make-next-button editor :size button-size)))
    ))



(defmethod update-after-prop-edit ((self spat-scene-editor) object)
  (editor-invalidate-views self)
  (report-modifications self))


(defmethod build-selected-source-items ((self spat-scene-editor))
  (let* ((selection (car (get-selected-timelines (timeline-editor self))))
         (traj (and selection (nth selection (trajectories (object-value self))))))
    (list
     (om-make-di 'om-simple-text :text "SELECTION" :font (om-def-font :font1b) :size (om-make-point 60 18))
     (om-make-layout
      'om-row-layout
      :subviews (list
                 (om-make-di 'om-simple-text :text "Name" :font (om-def-font :font1b) :size (om-make-point 60 18)
                             :color (if traj (om-def-color :black) (om-def-color :gray)))
                 (when traj (make-prop-item :string :name traj :default nil :update self))))
     (om-make-layout
      'om-row-layout
      :subviews (list
                 (om-make-di 'om-simple-text :text "Color" :font (om-def-font :font1b) :size (om-make-point 60 18)
                             :color (if traj (om-def-color :black) (om-def-color :gray)))
                 (when traj (make-prop-item :color :color traj :default nil :update self))))
     )))


(defmethod make-editor-window-contents ((editor spat-scene-editor))

  (let ((timeline-container (om-make-layout 'om-simple-layout))

        (mode-selector (om-make-di
                        'om-popup-list :items '(:spat :3dc)
                        :value (editor-get-edit-param editor :view-mode)
                        :size (omp 120 24)
                        :font (om-def-font :font1)
                        :di-action #'(lambda (b)
                                       (case (om-get-selected-item-index b)
                                         (0 (unless (equal (editor-get-edit-param editor :view-mode) :spat)
                                              (update-view-mode editor :spat)))
                                         (1 (unless (equal (editor-get-edit-param editor :view-mode) :3dc)
                                              (update-view-mode editor :3dc)))
                                         ))))

        (sources-control (om-make-layout
                          'om-row-layout
                          :subviews (list
                                     (om-make-di 'om-simple-text :text "SOURCES"
                                                 :font (om-def-font :font1b)
                                                 :size (om-make-point 50 18))

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
                                     ))))


    (set-g-component (timeline-editor editor) :main-panel timeline-container)
    (make-timeline-view (timeline-editor editor))

    (set-g-component
     editor
     :spat-view-container
     (om-make-layout
      'om-simple-layout
      :subviews (list
                 (if (equal (editor-get-edit-param editor :view-mode) :spat)
                     (setf (spat-view editor) (om-make-view 'spat-view :size (omp 200 200)))
                   (make-editor-window-contents (3dc-editor editor)))
                 )))

    (set-g-component
     editor
     :selection-params
     (om-make-layout 'om-column-layout
                     :bg-color (om-def-color :light-gray)
                     :subviews (build-selected-source-items editor)
                     ))

    (om-make-layout
     'om-row-layout
     :bg-color (spat-editor-bg-color editor)
     :align :center
     :ratios '(1 50 1)
     :subviews
     (list nil
           (om-make-layout
            'om-column-layout
            :ratios '(1 1 98 nil 1 1)
            :subviews (list
                       nil
                       mode-selector
                       (om-make-layout
                        'om-row-layout
                        :ratios '(99 1)
                        :subviews
                        (list (get-g-component editor :spat-view-container)
                              (om-make-layout
                               'om-column-layout
                               :subviews
                               (list (make-default-editor-view editor)
                                     NIL
                                     sources-control
                                     (get-g-component editor :selection-params)
                                     NIL
                                     ))
                              ))
                       :divider
                       timeline-container
                       nil)
            )
           nil))
    ))


(defmethod spat-view-init ((self spat-scene-editor))
  (case (editor-get-edit-param self :view-mode)
    (:spat
     (spat-editor-set-spat-component self))
    (:3dc
     (enable-multi-display (3DC-editor self) (trajectories (object-value self)))
     (init-editor-window (3DC-editor self)))
    ))


(defmethod update-view-mode ((editor spat-scene-editor) mode)

  (cond
   ((equal mode :spat)
    (om-remove-all-subviews (get-g-component editor :spat-view-container))

    (om-add-subviews
     (get-g-component editor :spat-view-container)
     (setf (spat-view editor) (om-make-view 'spat-view :size (omp 200 200))))

    (spat-editor-set-spat-component editor)
    (init-messages-to-spat-viewer editor)
    (activate-spat-callback editor))

   ((equal mode :3dc)
    (spat-editor-remove-spat-component editor)
    (om-remove-all-subviews (get-g-component editor :spat-view-container))
    (setf (spat-view editor) nil)

    (om-add-subviews
     (get-g-component editor :spat-view-container)
     (make-editor-window-contents (3dc-editor editor)))

    (setf (contents (object (3DC-editor editor)))
          (if (selected-sources editor)
              (nth (car (selected-sources editor)) (trajectories (object-value editor)))
            (car (trajectories (object-value editor)))))

    (enable-multi-display (3DC-editor editor) (trajectories (object-value editor)))
    (init-editor-window (3DC-editor editor))

    ))

  (editor-set-edit-param editor :view-mode mode)
  (update-display-contents editor)
  (report-modifications editor))



(defmethod update-3Dview-selection ((self spat-scene-editor))

  (om-remove-all-subviews (get-g-component (3dc-editor self) :selection-params))

  (om-add-subviews (get-g-component (3dc-editor self) :selection-params)
                   (build-selected-source-items self)))



;;; UPDATE EVERYTHING FROM THE CURRENT STATE
;;; (not optimal !!)
(defmethod update-display-contents ((editor spat-scene-editor))

  (when (window editor)

    (case (editor-get-edit-param editor :view-mode)

      (:3dc
       (editor-set-edit-param (3dc-editor editor) :background (make-bg-speakers (speakers (object-value editor))))
       (update-editor-3d-object (3dc-editor editor)))

      (:spat
       (when spat::*spat*
         (spat-osc-command
          (spat-GUI-component (spat-view editor))
          (append-set-to-state-messages
           (append (spat-scene-speakers-messages (object-value editor))
                   (spat-scene-sources-messages (object-value editor)
                                                (or (get-cursor-time (timeline-editor editor)) 0)
                                                (selected-sources editor))
                   ))
          (spat-view editor)))
       ))
    ))


;;; called from play-callback
(defmethod update-spat-display ((self spat-scene-editor))
  (update-display-contents self))


;;;=============================
;;; ADD / REMOVE SOURCES
;;;=============================

;;; called from the main GUI +/- buttons
(defmethod add-source ((self spat-scene-editor))
  (let* ((ss (object-value self))
         (last-traj-index (1- (length (trajectories ss))))
         (last-col (and (trajectories ss) (color (nth last-traj-index (trajectories ss)))))
         (new-traj (om-init-instance
                    (make-instance
                     '3DC
                     :x-points 0 :y-points 0 :z-points 0 :times 0
                     :color (and last-col (find-next-color-in-golden-palette last-col))
                     ))))

    (setf (audio-in ss) (append (audio-in ss) (list nil))) ; (number-to-string (+ last-traj-index 2)))))
    (setf (trajectories ss) (append (trajectories ss) (list new-traj)))

    (set-name new-traj (number-to-string (length (trajectories ss))))
    (make-timeline-view (timeline-editor self))

    ;;; select last-created timeline
    (let ((sel (list (1- (length (trajectories ss))))))
      (set-selected-timelines (timeline-editor self) sel)
      (select-source self (car sel)))

    (enable-play-controls self t)
    (update-source-picts self)
    (spat-object-set-audio-dsp ss)
    (update-display-contents self)
    (report-modifications self)
    ))

(defmethod remove-source ((self spat-scene-editor))
  (when (trajectories (object-value self))
    (let* ((ss (object-value self))
           (current-selection (or (get-selected-timelines (timeline-editor self))
                                  (list (1- (length (trajectories ss)))))) ;;; if no selection : will remove the last source
           )

      (loop for ns in current-selection do
            (setf (audio-in ss) (remove-nth ns (audio-in ss)))
            (setf (trajectories ss) (remove-nth ns (trajectories ss))))

      (make-timeline-view (timeline-editor self))
      (enable-play-controls self t)
      (set-selected-timelines (timeline-editor self) nil)
      ;(om-set-layout-ratios (main-view self) '(1 9 1))
      (update-display-contents self)
      (update-source-picts self)
      (spat-object-set-audio-dsp ss)
      (report-modifications self)
      )))


(defmethod select-source ((editor spat-scene-editor) (n number))

  (setf (selected-sources editor) (list n))

  (setf (selection (3DC-editor editor)) nil)

  (let ((abs-container (object (3DC-editor editor)))) ;; in principle this is an OMAbstractContainer
    (setf (contents abs-container)
          (nth (car (selected-sources editor)) (trajectories (object-value editor)))))

  ;;; update the 3DC-editor
  (enable-multi-display (3DC-editor editor) (trajectories (object-value editor)))

  (om-remove-all-subviews (get-g-component editor :selection-params))
  (apply #'om-add-subviews (cons (get-g-component editor :selection-params)
                                 (build-selected-source-items editor)))

  (editor-invalidate-views (3DC-editor editor)))


(defmethod editor-delete-contents-from-timeline ((self spat-scene-editor) timeline-id sel)
  (let ((traj (nth timeline-id (trajectories (object-value self)))))
    (mapcar #'(lambda (point) (time-sequence-remove-timed-item traj point)) sel)
    (time-sequence-update-internal-times traj))
  (editor-invalidate-views self)
  (report-modifications self))


;;; add a source audio file selector and a "mute" check box on the left of the timelines
(defmethod make-timeline-left-item ((self spat-scene-editor) id)
  (if id
      (let* ((src (nth id (audio-in (object-value self))))
             (cb (om-make-di
                  'om-check-box :size (omp 15 15)
                  :enable src
                  :checked-p (and src (not (mute src)))
                  :di-action #'(lambda (b)
                                 (when src (setf (mute src) (not (om-checked-p b))))
                                 (update-display-contents self))))
             (folder-b (om-make-graphic-object
                        'om-icon-button :size (omp 15 15) :position (omp 0 0)
                        :icon :folder-button :icon-pushed :folder-button-pushed
                        :lock-push nil
                        :action #'(lambda (b)
                                    (declare (ignore b))
                                    (let ((snd (om-init-instance (objFromObjs :choose-file (make-instance 'sound)))))
                                      (when snd
                                        (let* ((ss (object-value self))
                                               (traj (nth id (trajectories ss)))
                                               (point (make-default-tpoint-at-time traj (get-obj-dur snd))))
                                          (setf (nth id (audio-in ss)) snd)
                                          (time-sequence-insert-timed-item-and-update traj point))
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
        (call-next-method))))


;;;====================================
;;; SPAT MESSAGES
;;;====================================

(defmethod spat-object-init-GUI-messages ((editor spat-scene-editor))
  (append
   (call-next-method)
   '(("/layout" "single")
     ("/format" "xyz"))
   (spat-scene-speakers-messages (object-value editor))
   (spat-scene-sources-messages (object-value editor) 0)
   ))


(defmethod spat-scene-speakers-messages ((ss spat-scene))
  (append
   `(("/speaker/number" ,(length (speakers ss))))
   `(,(cons "/speakers/xyz" (apply #'append (speakers ss))))
   (loop for spk in (speakers ss) for n = 1 then (1+ n) collect
         (list (format nil "/speaker/~D/editable" n) 1))
   ))


(defmethod spat-scene-sources-messages ((ss spat-scene) at-time &optional selection)

  (append
   `(("/source/number" ,(length (audio-in ss))))

   (loop for traj in (trajectories ss)
         for n from 0
         append
         (let ((src (nth n (audio-in ss)))
               (address-prefix (format nil "/source/~D" (1+ n)))
               (pt (time-sequence-get-active-timed-item-at traj at-time))
               (col (or (color traj) (om-def-color :green)))) ;;; here goes the interpolation
           (when pt
             (list
              (list (string+ address-prefix "/xyz") (om-point-x pt) (om-point-y pt) (om-point-z pt))
              (list (string+ address-prefix  "/name") (format nil "~A" (1+ n)))
              (list (string+ address-prefix "/select") (if (find n selection) 1 0))
              (list (string+ address-prefix "/color")
                    (coerce (om-color-r col) 'single-float)
                    (coerce (om-color-g col) 'single-float)
                    (coerce (om-color-b col) 'single-float)
                    (if (and src (mute src)) 0.2 1.0)))
             ))
         )))


;;======================
;;SpatVIEW CALLBACK (UPDATES)
;;========================

(defmethod update-position-at-time ((self time-sequence) new-p time)
  (let ((p (point-exists-at-time self time)))
    (if p (om-point-set p :x (3dpoint-x new-p)
                        :y (3dpoint-y new-p)
                        :z (3dpoint-z new-p))
      (progn
        (om-point-set new-p :time time)
        (time-sequence-insert-timed-item-and-update self new-p)
        ))))

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
                              (remove n (get-selected-timelines (timeline-editor editor)) :test '=)))
    ))

(defun source-dbclick-callback (editor n) (declare (ignore editor n)) nil)

(defun speaker-moved-callback (editor n pos)
  (setf (nth n (speakers (object-value editor))) pos)
  (report-modifications editor))


(defun update-global-state (editor)
  (setf (car (controls (object-value editor)))
        (make-instance 'osc-bundle :date 0
                       :messages (filter-osc-messages (spat-get-state (spat-gui-component (spat-view editor)))
                                                      *excluded-spat-state-messages*)))
  ;;(report-modifications editor)
  )


;;; MAIN CALLBACK HANDLER
(defmethod spat-callback-to-front-editor ((editor spat-scene-editor) messages)

  (let ((keypressed (find "/keypressed" messages :key 'car :test 'string-equal)))

    (if keypressed

        (let ((key (caddr keypressed))
              ; (modifier (cadr keypressed))
              )
          (editor-key-action editor (code-char key)))

      (loop for mess in messages do

            (let* ((address-tokens (cdr (string-to-list (car mess) "/")))
                   (obj (car address-tokens)))

              (cond

               ((string-equal obj "source")
                (let ((n (read-from-string (cadr address-tokens)))
                      (action (caddr address-tokens)))
                  (cond ((string-equal action "select")
                         (let ((val (cadr mess)))
                           (source-selected-callback editor (1- n) val)))
                        ((string-equal action "xyz")
                         (source-moved-callback editor (1- n) (cdr mess)))
                        ((string-equal action "doubleclick")
                         (source-dbclick-callback editor (1- n)))
                        (t (update-global-state editor)))
                  ))

               ((string-equal obj "room")
                (update-global-state editor))

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
;; PLAY
;;===================

(defmethod play-editor-callback ((self spat-scene-editor) time)
  (play-editor-callback (3dc-editor self) time)
  (call-next-method))

(defmethod editor-play ((self spat-scene-editor))
  (call-next-method)
  (loop for tr in (trajectories (object-value self))
        do
        (setf (state tr) :play) ;; for display
        (setf (ref-time tr) (ref-time (object-value self)))
        ))


(defmethod editor-stop ((self spat-scene-editor))
  (call-next-method)
  (loop for tr in (trajectories (object-value self))
        do (setf (state tr) :stop))
  (update-display-contents self)
  (update-timeline-display self))

(defmethod editor-reset-interval ((self spat-scene-editor))
  (call-next-method)
  (update-display-contents self)
  (update-timeline-display self))


;;===================
;; KEY
;;===================

(defmethod editor-key-action ((editor spat-scene-editor) key)
  (case key
    (:om-key-esc
     (set-selected-timelines (timeline-editor editor) nil))
    (otherwise nil)
    )
  (or (editor-key-action (3dc-editor editor) key)
      (call-next-method)
      )
  )





