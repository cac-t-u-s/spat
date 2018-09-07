
(in-package :om)

(require-om-package "sdif")
(require-om-package "sound")

;;;===============================
;;; OSC binding for Spat Component
;;;===============================
(defun spat-osc-command (component-ptr messages &optional apply-in-view)
    
  (when messages 
   
    (if apply-in-view
        
        ;;; call itself again, but in another thread
        (capi:apply-in-pane-process apply-in-view 'spat-osc-command component-ptr messages) 
      
      (let ((osc-ptr (make-foreign-bundle-s-pointer messages))) ;; (ob (make-o.bundle messages))
        
        (om-print-dbg 
         "[~A] ~A/~A ~{~%                         <= ~A ~}" 
         (list (remove #\~ (spat::OmSpatGetComponentType component-ptr)) component-ptr apply-in-view messages)
         "OM-SPAT")
        
        (unwind-protect
            (spat::OmSpatProcessOscCommands component-ptr osc-ptr)
          (odot::osc_bundle_s_deepFree osc-ptr))
        )
      )
    ))
      
(defun spat-get-state (component-ptr)
  (let ((state-bundle (spat::OmSpatGetCurrentStateAsOscBundle component-ptr)))
    (unwind-protect 
        (om::decode-bundle-s-pointer-data state-bundle)
      (odot::osc_bundle_s_deepFree state-bundle))))

(defun spat-get-dsp-commands (component-ptr)
  (let ((state-bundle (spat::OmSpatGetBundleFromGuiToDsp component-ptr)))
    (unwind-protect 
        (om::decode-bundle-s-pointer-data state-bundle)
      (odot::osc_bundle_s_deepFree state-bundle))))

;; (spat-osc-command spat '(("/panning/type" "binaural")))


;;; TEST FUNCTION (not used anywhere eslse)
;;; in-buffer and out-buffer come from / is returned to the environment
;;; all the rest is freed 
(defmethod spat-process-sound ((self sound) spat-comp n-speakers (oscb osc-bundle))
  (if (spat::omspatisvalidcomponenttype spat-comp)
    (with-audio-buffer (in-buffer self)
      (when in-buffer ;;; guarantees that the audio samples are temporaily loaded in b
        
        (let ((spat (spat::OmSpatCreateDspComponentWithType spat-comp (n-channels self) n-speakers))
              (out-buffer (fli:allocate-foreign-object :type :pointer :nelems n-speakers)))
          
          ;;; allocate the out buffer channels
          (dotimes (ch n-speakers)
            (setf (fli:dereference out-buffer :index ch :type :pointer)
                  (fli:allocate-foreign-object :type :float :nelems (n-samples self) :initial-element 0.0)))
    
          (let ((spat-in (spat::allocate-spat-audiobuffer :channels (n-channels self) :size (n-samples self) :data (om-sound-buffer-ptr in-buffer)))
                (spat-out (spat::allocate-spat-audiobuffer :channels n-speakers :size (n-samples self) :data out-buffer)))
          
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
      
                  (unless (spat::OmSpatProcessAudio spat spat-out spat-in (n-samples self))
                    (error "ERROR IN SPAT DSP"))
                
                  (when out-buffer ;;; now contains spatialized audio tracks
                    (let ((out-snd (make-instance 'om-internal-sound
                                                  :n-samples (n-samples self) :sample-rate (sample-rate self)
                                                  :n-channels n-speakers)))
                      (setf (buffer out-snd) (make-om-sound-buffer :ptr out-buffer :count 1 :nch n-speakers))
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


