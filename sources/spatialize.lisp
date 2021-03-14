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

;================================================
; Low-level spat commands: process audio buffers
; @author: J. Bresson
;================================================

(in-package :om)

;;;===============================
;;; OSC binding for Spat Component
;;;===============================
(defun spat-osc-command (component-ptr messages &optional apply-in-view)

  (when messages

    (if apply-in-view

        ;;; call itself again, but in another thread
        (capi:apply-in-pane-process apply-in-view 'spat-osc-command component-ptr messages)

      (let ((osc-ptr (odot::osc_make_foreign_bundle_s messages))) ;; (ob (make-o.bundle messages))

        ; for real-real debug !
        ;(om-print-dbg
        ; "[~A] ~A/~A ~{~%                         <= ~A ~}"
        ; (list (remove #\~ (spat::OmSpatGetComponentType component-ptr)) component-ptr apply-in-view messages)
        ; "OM-SPAT-DEBUG")

        (unwind-protect
            (spat::OmSpatProcessOscCommands component-ptr osc-ptr)
          (odot::osc_bundle_s_deepFree osc-ptr))
        )
      )
    ))

(defun spat-get-state (component-ptr)
  (let ((state-bundle (spat::OmSpatGetStateAsOscBundle component-ptr)))
    (unwind-protect
        (odot::osc_decode_bundle_s_data state-bundle)
      (odot::osc_bundle_s_deepFree state-bundle))))

(defun spat-get-dsp-commands (component-ptr)
  (let ((state-bundle (spat::OmSpatGetBundleFromGuiToDsp component-ptr)))
    (unwind-protect
        (odot::osc_decode_bundle_s_data state-bundle)
      (odot::osc_bundle_s_deepFree state-bundle))))

;; (spat-osc-command spat '(("/panning/type" "binaural")))


;; (spat::omspatisvalidcomponenttype "spat.decoder~")

;;; in-buffer and out-buffer come from / is returned to the environment
;;; all the rest is freed
(defmethod! spat-process ((input sound) (spat-comp string) (n-outputs integer) (oscb osc-bundle) &optional to-file)
  :icon :spat
  :initvals '(nil "spat5.pan~" 2 nil nil)
  :indoc '("a sound object" "the string designating a spat DSP component" "number of output channels" "an OSC-bundle")
  :doc "Processes <input> with <spat-comp> and the spat control messages in <oscb>.

<spat-comp> can be any valid spat5.*~ DSP object name: spat5.cascade~, spat5.pan~, spat5.compressor~, etc.

If input is a multi-channel audio file, is channel is treated as a source for the spat DSP object.
"
  (if (spat::omspatisvalidcomponenttype spat-comp)
      (with-audio-buffer (in-buffer input)
        (when in-buffer ;;; guarantees that the audio samples are temporaily loaded in b

          (let ((spat (spat::OmSpatCreateDspComponentWithType spat-comp (n-channels input) n-outputs))
                (out-buffer (fli:allocate-foreign-object :type :pointer :nelems n-outputs)))

            ;;; allocate the out buffer channels
            (dotimes (ch n-outputs)
              (setf (fli:dereference out-buffer :index ch :type :pointer)
                    (fli:allocate-foreign-object :type :float :nelems (n-samples input) :initial-element 0.0)))

            (let ((spat-in (spat::allocate-spat-audiobuffer :channels (n-channels input) :size (n-samples input) :data (om-sound-buffer-ptr in-buffer)))
                  (spat-out (spat::allocate-spat-audiobuffer :channels n-outputs :size (n-samples input) :data out-buffer)))

              (unwind-protect
                  (handler-bind ((error #'(lambda (e)
                                            (print (format nil "~A" e))
                                            (print (spat::OmSpatGetLastError))
                                            (spat::OmSpatClearLastError)
                                            (abort e))))

                    (unless (spat-osc-command spat (messages oscb))
                      (error "ERROR IN SPAT OSC-CONTROL PROCESSING"))

                    ;(print (spat::OmSpatGetComponentType spat))
                    ;(print (spat-get-state spat))

                    (unless (spat::OmSpatProcessAudio spat spat-out spat-in (n-samples input))
                      (error "ERROR IN SPAT DSP"))

                    (when out-buffer ;;; now contains spatialized audio tracks
                      (let ((out-snd (make-instance 'sound :buffer (make-om-sound-buffer :ptr out-buffer :count 1 :nch n-outputs)
                                                    :n-samples (n-samples input)
                                                    :n-channels n-outputs
                                                    :sample-rate (sample-rate input))
                                     ))

                        (when to-file
                          (unless (pathname-directory to-file)
                            (setf to-file (outfile to-file)))
                          (setf (file-pathname out-snd) (handle-new-file-exists to-file))
                          (setf (access-from-file out-snd) t))

                        out-snd))
                    )
                ;;; cleanup forms
                (spat::free-spat-audiobuffer spat-in)
                (spat::free-spat-audiobuffer spat-out)
                (spat::omspatfreecomponent spat)
                )
              )
            )))
    (om-beep-msg "Invalid Spat component: ~A" spat-comp)
    ))
