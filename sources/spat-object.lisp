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

;==============================================================
; SPAT-OBJECT : abstract superclass for SPAT-DSP and SPAT-SCENE
; @author: J. Bresson
;==============================================================

(in-package :om)

(defclass spat-object (om-cleanup-mixin named-object schedulable-object object-with-action)
  ((audio-in :accessor audio-in :initform nil :initarg :audio-in :documentation "audio input") ; sound or list of sounds
   (controls :initarg :controls :accessor controls :initform nil :documentation "list of timed OSC-bundles")
   
   (audio-sr :accessor audio-sr :initform 44100)
   (buffer-size :accessor buffer-size :initform 8192)
   
   (spat-processor :accessor spat-processor :initform nil)
   (spat-controller :accessor spat-controller :initform nil)

   (in-buffer :accessor in-buffer :initform nil)
   (out-buffer :accessor out-buffer :initform nil)
   (buffer-player :accessor buffer-player :initform nil)
   ))

;;; we don't want to copy these slots
;;; ?? this is probably not necessary are the slots are neither :initargs or adiitional-class-attributes...
(defmethod excluded-slots-from-copy ((from spat-object)) 
  '(spat-processor spat-controller in-buffer out-buffer buffer-player))

(defmethod additional-class-attributes ((self spat-object)) '(audio-sr buffer-size))

(defmethod SpatDSPComponent-name ((self spat-object)) nil)
(defmethod SpatControllerComponent-name ((self spat-object)) nil)

(defmethod n-channels-in ((self spat-object)) 
  (apply '+ (mapcar 'n-channels (list! (audio-in self)))))

(defmethod n-channels-out ((self spat-object)) 
  (n-channels-in self))

(defmethod get-obj-dur ((self spat-object)) 
  (reduce 'max 
          (or (remove nil (mapcar 'get-obj-dur (list! (audio-in self))))
              '(1000))))

(defmethod play-obj? ((self spat-object)) t)

(defmethod get-controller-state ((self spat-object))
  (when (spat-controller self)
    (make-instance 
     'osc-bundle :date 0
     :messages (spat-get-state (spat-controller self)))))

(defmethod get-dsp-state ((self spat-object))
  (when (spat-processor self)
    (make-instance 
     'osc-bundle :date 0
     :messages (spat-get-state (spat-processor self)))))


;;; create a complete control bundle at t=0
(defmethod ensure-init-state ((self spat-object))
  (unless (and (controls self) (= 0 (date (car (controls self)))))
    (let ((init (get-controller-state self)))
      (when init
        (setf (controls self) (cons init (controls self)))))))


(defun append-set-to-state-messages (messages)
  (loop for msg in messages 
        collect (cons (string+ "/set" (car msg)) (cdr msg))))

(defun filter-osc-messages (messages addresses)
  (remove-if #'(lambda (msg) 
                 (find (car msg) addresses 
                       :test #'(lambda (a b) (lw::find-regexp-in-string b a))))
             messages))


;;; better use om-init-instance ?
;;; ... would solve a few things, e.g. in initializing spat-processors with panning-type in spat-scene

;(defmethod initialize-instance :after ((self spat-object) &rest initargs)
(defmethod om-init-instance ((self spat-object) &optional args)

  ; time-window determines the pre-delay and the rate of the calls from the scheduler 
  ; maybe better to round it ?
  
  (if (find-value-in-kv-list args :audio-in)
      (setf (audio-in self)
            (loop for s in (list! (find-value-in-kv-list args :audio-in))
                  collect (get-sound (om-copy s)))
            )
    (setf (audio-in self) (list! (audio-in self))))
  
  (set-object-time-window self (* 4 (samples->ms (buffer-size self) (audio-sr self)))) 
  (spat-object-set-audio-dsp self)
  (spat-object-set-spat-controller self)
  
  (ensure-init-state self)
  
  ; we'll do this only if necessary before play
  ;(when (equal (action self) 'render-audio) (set-play-buffer self))
  
  self)

(defmethod buffer-size-accessor ((self spat-object) &optional (value nil value-supplied-p))
  (if value-supplied-p
      (progn (setf (buffer-size self) value)
        (spat-object-set-audio-dsp self))
    (buffer-size self)))

(defmethod action-accessor ((self spat-object) &optional (value nil value-supplied-p))
  (if value-supplied-p
      (set-action self value)
    (action self)))


;(defmethod om-init-instance ((self spat-object) &optional args)
;  (let ((ss (call-next-method)))
;    (when (and (find-value-in-kv-list args :buffer-size)
;               (not (= (find-value-in-kv-list args :buffer-size) 
;                       (buffer-size self))))
;      (setf (buffer-size ss) (find-value-in-kv-list args :buffer-size))
;      (spat-object-set-audio-dsp self))
;    ss))

;(defmethod clone-object ((self spat-object) &optional clone)
;  (let ((copy (call-next-method)))
;    (set-play-buffer copy)
;    copy))


(defmethod spat-object-set-audio-dsp ((self spat-object))
  (player-stop-object om::*scheduler* self)
  (spat-object-free-audio-dsp self)
  (set-audio-buffers self)
  (set-spat-processor self)
  (spat-osc-command (spat-processor self) (spat-object-init-DSP-messages self))
  ;(set-play-buffer self)
  )

(defmethod spat-object-set-spat-controller ((self spat-object))
  (spat-object-free-spat-controller self)
  (set-spat-controller self))


(defmethod spat-object-free-audio-dsp ((self spat-object))
  (when (spat-processor self)
    (om-print-dbg "~A: Free SPAT-Processor ~A in ~A" (list (type-of self) (spat-processor self) self) "OM-SPAT-DEBUG") 
    (spat::OmSpatFreeComponent (spat-processor self)))
  (when (in-buffer self) 
    (spat::OmSpatFreeAudioBuffer (in-buffer self))
    (setf (in-buffer self) nil))
  (when (out-buffer self)
    (spat::OmSpatFreeAudioBuffer (out-buffer self))
    (setf (out-buffer self) nil))
  (when (buffer-player self) 
    (free-buffer-player (buffer-player self))
    (setf (buffer-player self) nil)))
  

(defmethod spat-object-free-spat-controller ((self spat-object))
  (when (spat-controller self)
    (om-print-dbg "~A: Free SPAT-Controller ~A in ~A" (list (type-of self) (spat-controller self) self) "OM-SPAT-DEBUG")
    (spat::OmSpatFreeComponent (spat-controller self))
    ))


;;; from om-cleanup-mixin
(defmethod om-cleanup ((self spat-object))
  (spat-object-free-spat-controller self)
  (spat-object-free-audio-dsp self))


(defmethod set-spat-controller ((self spat-object))
  (let ((ctrl-comp-name (SpatControllerComponent-name self)))
    (when ctrl-comp-name
      (if (spat::omspatisvalidcomponenttype ctrl-comp-name)
          (let ((comp (spat::OmSpatCreateComponentWithType ctrl-comp-name)))
            (om-print-dbg "~A: Create spat-controller ~A [~A] in ~A" (list (type-of self) comp (remove #\~ ctrl-comp-name) self) "OM-SPAT-DEBUG")
            (setf (spat-controller self) comp))
        (om-beep-msg "OM-SPAT: Wrong GUI component: ~A" ctrl-comp-name)) 
      )))


(defmethod set-spat-processor ((self spat-object))
  (let ((n-out (n-channels-out self))
        (n-in (n-channels-in self))
        (dsp-comp-name (SpatDSPComponent-name self)))
    (when dsp-comp-name
      (if (spat::omspatisvalidcomponenttype dsp-comp-name)
          (progn 
            (om-print-dbg "~A: Create spat-processor ~A [~A] in ~A" 
                          (list (type-of self) (spat-processor self) (remove #\~ dsp-comp-name) self) "OM-SPAT-DEBUG")
            (setf (spat-processor self) (spat::OmSpatCreateDspComponentWithType dsp-comp-name n-in n-out))
            )
        (om-beep-msg "OM-SPAT: Wrong DSP component: ~A" dsp-comp-name)) 
      )))


(defmethod set-audio-buffers ((self spat-object))
  (let* ((buffersize (buffer-size self))
         (nsrc (n-channels-in self))
         (nch (n-channels-out self)))

   (when (> nsrc 0)

     (let ((inptr (fli:allocate-foreign-object :type :pointer :nelems nsrc)))
       (om-print-dbg "Initializing Spat input buffers (~Ax~A)." (list nsrc buffersize) "OM-SPAT-DEBUG")    
        
        (dotimes (src nsrc)
          (setf (fli:dereference inptr :index src :type :pointer);(cffi::mem-aref inptr :pointer src) 
                (fli:allocate-foreign-object :type :float :nelems buffersize :initial-element 0.0)))
        
        (setf (in-buffer self) (spat::allocate-spat-audiobuffer :channels nsrc :size buffersize :data inptr))))
    
    (when (> nch 0)
      (let ((outptr (fli:allocate-foreign-object :type :pointer :nelems nch)))
        (om-print-dbg "Initializing Spat output buffers (~Ax~A)." (list nch buffersize) "OM-SPAT-DEBUG")
        
        (dotimes (ch nch)
          (setf (fli:dereference outptr :index ch :type :pointer);(cffi::mem-aref outptr :pointer ch) 
                (fli:allocate-foreign-object :type :float :nelems buffersize :initial-element 0.0)))
        
        (setf (out-buffer self) (spat::allocate-spat-audiobuffer :channels nch :size buffersize :data outptr))))
    
    ))
    

;;; spat-object can play up to 1 minute of audio
; (defparameter *spat-object-player-size-ms* 60000)

; (+ (get-obj-dur self) 1000)

(defmethod set-play-buffer ((self spat-object))  
  (let* ((nch (n-channels-out self)) ;; depends on n-speakers / binaural mode
         (size (ms->samples (+ (get-obj-dur self) 1000) (audio-sr self))))
    
    (om-print-dbg "Initializing player buffer (~Ax~A)." (list nch size) "OM-SPAT-DEBUG")

    (when (> nch 0)
      (let ((audio-buffer 
             (fli::allocate-foreign-object 
              :type :pointer :nelems nch
              ;:initial-contents (loop for c from 0 to (1- nch) collect
              ;                        (fli::allocate-foreign-object :type :float :nelems size :initial-element 0.0))
              )))
        (dotimes (ch nch)
          (setf (fli:dereference audio-buffer :index ch :type :pointer) 
                (fli:allocate-foreign-object :type :float :nelems size :initial-element 0.0)))
        
        (setf (buffer-player self) 
              (make-player-from-buffer audio-buffer size nch (audio-sr self)))
        ))
    ))




;;;=====================
;;; CALL DSP
;;;=====================

(defmethod spat-object-init-DSP-messages ((self spat-object)) 
  `(("/dsp/samplerate" ,(float (audio-sr self)))))

;;; !! TO-FROM must be <= WINDOW-SIZE !!
(defun spat-object-process-dsp (self from-smp to-smp)
  ;(om-print (format nil "Preparing audio from ~D to ~D" from-smp to-smp) "SPAT-SCENE")
  ;;; FILL IN-BUFFER WITH SOURCE SECTION
  (if (or (null (in-buffer self))
          (null (out-buffer self)))
      
      (om-print "Spat audio buffers not initialized !" "OM-SPAT"))
  
  (let ((n-samples (1+ (- to-smp from-smp)))
        (inptr (spat::spat-audiobuffer-get-data (in-buffer self)))
               ;(buffer-size (ms->samples (time-window self) (audio-sr self)))
        (buffer-size (buffer-size self))
        (n 0))
    
    (if (< (buffer-size self) n-samples)
        
        (om-beep-msg "Window [~D-~D] is too big for the SPAT-OBJECT buffer size (~D)" from-smp to-smp buffer-size)
    
      (let ()
    ;(om-print (format nil "Get ~D samples from ~D" n-samples from-smp) "SPAT-SCENE-PLAYER")
        ;;; FILL THE SPAT IN-POINTER WITH SOURCES
        (when (>= from-smp 0)
          (loop for src in (audio-in self)
                do (when src 
                     (let ((b (buffer src))) 
                       (when (and b (om-sound-buffer-ptr b)) ;;; if not = problem...
                         (loop for ch from 0 to (1- (n-channels src)) do  
                               (if (mute src) 
                                   (dotimes (i n-samples)
                                     (setf (fli:dereference (fli:dereference inptr :index n :type :pointer) :index i :type :float) 0.0))
                                 (dotimes (i n-samples)
                                   (setf (fli:dereference (fli:dereference inptr :index n :type :pointer) :index i :type :float)
                                         (if (< (+ from-smp i) (n-samples src))
                                             (fli:dereference 
                                              (fli:dereference (om-sound-buffer-ptr b) :index 0 :type :pointer)
                                              :index (+ from-smp i) :type :float)
                                           0.0))))
                               (setf n (1+ n)))
                         ))
                     )))
           
    ;(spat::omspatprintaudiobuffer (in-buffer self) nil)
    ;(spat::omspatprintaudiobuffer (out-buffer self) nil)
    
        (handler-bind ((error #'(lambda (e) 
                                  (print (format nil "!!! ~A" e))
                                  (print (spat::OmSpatGetLastError))
                                  (spat::OmSpatClearLastError)
                                  (abort e))))
      
          ;;; SEND SPAT CONTROL COMMANDS AT
          ;;; TIME = BEGINNING OF THE WINDOW ONLY 
          (let* ((time-ms (samples->ms from-smp (audio-sr self)))
                 (messages (spat-object-get-process-messages-at-time self time-ms)))
            
            (when messages 
              
              (when (= time-ms 0)
                (setq messages (append-set-to-state-messages messages)))
              
              (unless (spat-osc-command (spat-processor self) messages)
                (error "ERROR IN SPAT CONTROL-MESSAGE PROCESSING"))
              ))
        
          ;;; PROCESS
          (if (spat::OmSpatProcessAudio (spat-processor self) (out-buffer self) (in-buffer self) n-samples)
              (spat::spat-audiobuffer-get-data (out-buffer self))
            (om-beep-msg "ERROR IN SPAT DSP")))
        "done")
      ))
  )


(defun spat-object-copy-output-to-buffer (self audio-buffer n-samples at)
  (when 
      (out-buffer self)
    (let ((out-buffer-data (spat::spat-audiobuffer-get-data (out-buffer self))))
      (dotimes (c (n-channels-out self))
        (dotimes (smp n-samples)
          (setf (fli:dereference (fli:dereference audio-buffer :index c :type :pointer)
                                 :index (+ at smp) :type :float)
                (fli:dereference (fli:dereference out-buffer-data :index c :type :pointer) :index smp :type :float)
                ))))))


;;;================
;;; OFFLINE SYNTHESIS
;;;================

(defmethod! spat-synth ((self spat-object) &optional to-file)  
  :icon :spat
  :indoc '("a spat object" "an output pathname")
  :doc "Processes the sources and DSP parameters as specified in <self> using Spat.

if <to-file> is NIL the generated sound is just stored in a buffer in RAM.
If it contains a pathname, the sound in store on a file a this location."
 
  (let* ((sp (om-copy self))
         (buffer-size (buffer-size sp))
         (total-size (ms->samples (get-obj-dur sp) (audio-sr sp)))
         (out-buffer (make-om-sound-buffer-gc 
                      :nch (n-channels-out sp)
                      :ptr (make-audio-buffer (n-channels-out sp) total-size))))
    
    (spat-osc-command (spat-processor self) '(("/dsp/clear")))
    (spat-osc-command (spat-processor self) (spat-object-init-DSP-messages self))
    ;;; messages from the first bundle (with "/set/...")
    (spat-osc-command (spat-controller self)  (append-set-to-state-messages (messages (car (controls self)))))
    (spat-osc-command (spat-processor self) (append-set-to-state-messages (spat-get-dsp-commands (spat-controller self))))

    (loop for smp = 0 then (+ smp buffer-size)
          while (< smp total-size) do

          (let ((smp2 (min total-size (1- (+ smp buffer-size)))))
            
            (om-print-dbg "Processing samples ~A to ~A" (list smp smp2) "OM-SPAT")  
            (spat-object-process-dsp sp smp smp2)
            
            (spat-object-copy-output-to-buffer 
             sp (om-sound-buffer-ptr out-buffer)
             (1+ (- smp2 smp)) smp)
            ))
  
    (om-print-dbg "Done! (~A channels)" (list (n-channels-out sp)) "OM-SPAT")
    
    (let ((sound (make-instance 'sound :buffer out-buffer
                                :n-samples total-size
                                :n-channels (n-channels-out sp) 
                                :sample-rate (audio-sr sp)
                                )))
      
      (when to-file 
        (unless (pathname-directory to-file) 
          (setf to-file (outfile to-file)))
        (setf (file-pathname sound) (handle-new-file-exists to-file))
        (setf (access-from-file sound) t))
      
      (om-init-instance sound)
      sound)
    ))




;;;===============================================
;;; PLAY
;;;===============================================

(defmethod get-def-action-list ((object spat-object))
  '(print send-spat-osc render-audio))

;;; dummy action, just to be accepted by object-with-action
(defun render-audio ())

(defmethod arguments-for-action ((fun (eql 'send-spat-osc)))
  '((:string host "localhost")
    (:int port 3000)))

(defun send-spat-osc (item &optional (host "localhost") (port 3000))
  (osc-send item host port))


(defmethod get-action-list-for-play ((self spat-object) interval &optional parent)
  (when (action self)
    (if (equal (action self) 'render-audio)
        (external-player-actions self interval parent)
      (spat-object-actions self interval)
      )))

;;; returns this in 'render-audio' mode 
(defmethod get-computation-list-for-play ((self spat-object) &optional interval)
  (when (equal (action self) 'render-audio)
    (spat-object-audio-actions self (car interval) (cadr interval))))

;;; to redefine by subclasses
(defmethod spat-object-actions ((self spat-object) interval) nil)
(defmethod spat-object-get-process-messages-at-time ((self spat-object) time-ms) nil)


;;;============================== 
;;; PLAY IN AUDIO MODE 
;;;============================== 

;;; called periodically by the scheduler in 'render-audio' mode 
#|
(defmethod spat-object-audio-actions ((self spat-object) t1 t2)
  (if (>= t2 *spat-object-player-size-ms*) (print "out of memory !!!!")
    (let* ((sr (audio-sr self))
           (smp1 (ms->samples t1 sr))
           (smp2 (1- (ms->samples t2 sr))))
 
      (list (list (- t1 (time-window self)) ;;; start time for computation
                  t1  ;;; deadline 
                  #'(lambda ()
                      ;(print (list "======================== actions of " smp1 smp2))
                      (when (buffer-player self)
                        (loop for s0 from smp1 to smp2 by (buffer-size self) 
                              do
                              (when  (spat-object-process-dsp self s0 (+ s0 (1- (buffer-size self)))))
                              ;;; COPY IN BUFFER-PLAYER BUFFER
                              ;(print (list "=> " s0))
                              (spat-object-copy-output-to-buffer  
                               self 
                               (bp-buffer (buffer-player self))
                               (buffer-size self) s0)
                        ))
                      )))
      )))
|#

(defmethod spat-object-audio-actions ((self spat-object) t1 t2)
   (let ((sr (audio-sr self)))
     (when (buffer-player self)
       (if (>= (ms->samples t2 sr) (bp-size (buffer-player self))) 
           (progn (om-print-dbg "Spat-object: player-buffer out of range... !!" nil "OM-SPAT")
             nil)
         (let ((smp1 (ms->samples t1 sr))
               (smp2 (1- (ms->samples t2 sr))))
           
           ;;; ???
           ;(when (= t1 0) 
           ;  (spat-osc-command (spat-processor self) '(("/dsp/clear")))
           ;  (spat-osc-command (spat-processor self) (spat-object-init-DSP-messages self))
           ;  ;;; messages from the first bundle (with "/set/...")
           ;   ;(spat-osc-command (spat-processor object) (append-set-to-state-messages (spat-get-dsp-commands (spat-controller object))))
           ;  (spat-osc-command (spat-processor self)  (spat-object-get-process-messages-at-time self 0)))
    
           (loop for s0 from smp1 to smp2 by (buffer-size self) 
                 do
                 (when (spat-object-process-dsp self s0 (+ s0 (1- (buffer-size self)))))
                 (spat-object-copy-output-to-buffer  
                  self 
                  (bp-buffer (buffer-player self))
                  (buffer-size self) s0)
                 ))))
     nil))


(defmethod player-play-object ((self scheduler) (object spat-object) caller &key parent interval)
  (declare (ignore parent))
  (call-next-method)
  
  (when (equal (action object) 'render-audio)
    
    (unless (buffer-player object) (set-play-buffer object))
    
    (when (buffer-player object)
      (jump-to-frame (buffer-player object) 0)
      (start-buffer-player (buffer-player object) 
                           :start-frame (if (car interval) (round (* (car interval) (/ (audio-sr object) 1000.0))) 0))))
  )

(defmethod player-stop-object ((self scheduler) (object spat-object))
  (when (and (equal (action object) 'render-audio)
             (buffer-player object))
    (stop-buffer-player (buffer-player object))
    (spat-osc-command (spat-processor object) '(("/dsp/clear"))))
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object spat-object))
  (when (and (equal (action object) 'render-audio)
             (buffer-player object))
    (pause-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object spat-object))
  
  (unless (buffer-player object) (set-play-buffer object))
  
  (when (and (equal (action object) 'render-audio)
             (buffer-player object))
    (continue-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod set-object-time ((self spat-object) time)
  (when (and (equal (action self) 'render-audio)
             (buffer-player self))   
    (jump-to-time (buffer-player self) time))
  (call-next-method))





