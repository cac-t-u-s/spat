;/************************************************************************************/
;/*  FILE DESCRIPTION							             */
;/*----------------------------------------------------------------------------------*/
;/*!
; *   @file       omspat-utils.lisp
; *   @brief      lisp-interface to OMSpat.framework Utilities
; *   @author     Thibaut Carpentier
; *   @version    $(PRODUCT_VERSION)
; *   @date       05/11/2013
; *
; */
;/************************************************************************************/

(in-package :spat)

;;;================================================================================
;;; GENERAL
;;;================================================================================

(cffi:defcfun ("OmSpatInitialize" OmSpatInitialize) :boolean)
(cffi:defcfun ("OmSpatIsInitialized" OmSpatIsInitialized) :boolean)
(cffi:defcfun ("OmSpatGetVersion" OmSpatGetVersion) :string)
(cffi:defcfun ("OmSpatGetLastError" OmSpatGetLastError) :string)
(cffi:defcfun ("OmSpatClearLastError" OmSpatClearLastError) :pointer)
(cffi:defcfun ("OmSpatGetGlobalSamplingRate" OmSpatGetGlobalSamplingRate) :float)
(cffi:defcfun ("OmSpatSetGlobalSamplingRate" OmSpatSetGlobalSamplingRate) :void (samplerate :float))
(cffi:defcfun ("OmSpatSetVerbose" OmSpatSetVerbose) :void (verbose :boolean))
(cffi:defcfun ("OmSpatIsVerbose" OmSpatIsVerbose) :boolean)

;(OmSpatIsInitialized)
;(OmSpatInitialize)
;(OmSpatGetVersion)
;(OmSpatGetLastError)
;(OmSpatIsVerbose)
;(OmSpatSetVerbose nil)

(defun test-osc-command (messages)
  (let ((ob (om::make-o.bundle (make-instance 'om::osc-bundle :messages (print messages)))))
    (spat::OmSpatDebugOSCPacket (om::o.bundle-ptr ob) (om::o.bundle-size ob))))

; (test-osc-command '(("/numsources" 4)))

;;;================================================================================
;;; UTILS
;;;================================================================================

(defun number-to-double (i) (coerce i 'double-float))
(defun number-to-float (i) (coerce i 'float))

(defun coerce-list (liste type)
  (case type 
    (:double (mapcar #'number-to-double liste))
    (:float (mapcar #'number-to-float liste))
    (otherwise nil)))

; Takes a list from OM and converts it into a C array. 
; The C-array is allocated here, but not freed
(defun lisp-list-to-c-array (liste &optional (type :float)) 
  (cffi::foreign-alloc :float :initial-contents (coerce-list liste type)))

; Takes a C-array and converts it into a lisp list
; The pointer is not freed here.
(defun c-array-to-lisp-list (ptr size &optional (type :float))
  (loop for i from 0 to (- size 1) collect (cffi::mem-aref ptr type i)))


;;;================================================================================
;;; AUDIOBUFFER
;;;================================================================================

(cffi:defcstruct omspat-audiobuffer 
  (numChannels :unsigned-int) 
  (numSamples :unsigned-long)
  (data :pointer))

(cffi::defctype omspat-audiobuffer_T (:struct omspat-audiobuffer))

;;; we suppose that **DATA is already allocated
(defun allocate-spat-audiobuffer (&key (size nil) 
                                       (channels nil) 
                                       (data nil))
  (let ((audiob (cffi::foreign-alloc 'omspat-audiobuffer_T)))
    (when channels (setf (cffi:foreign-slot-value audiob 'omspat-audiobuffer_T 'numChannels) channels))
    (when size (setf (cffi:foreign-slot-value audiob 'omspat-audiobuffer_T 'numSamples) size))
    (when data (setf (cffi:foreign-slot-value audiob 'omspat-audiobuffer_T 'data) data))
    audiob))

;;; we do not free DATA
(defun free-spat-audiobuffer (audiob)
  ;;;(cffi::foreign-free (cffi:foreign-slot-value audiob '(:struct spat::omspat-audiobuffer) 'data))
  (cffi::foreign-free audiob))

(defun spat-audiobuffer-set-data (audiob ptr)
  (setf (cffi:foreign-slot-value audiob 'omspat-audiobuffer_T 'data) ptr))

(defun spat-audiobuffer-get-data (audiob)
  (cffi:foreign-slot-value audiob 'omspat-audiobuffer_T 'data))

(defun spat-audiobuffer-get-n-samples (audiob)
  (cffi:foreign-slot-value audiob 'omspat-audiobuffer_T 'numsamples))

; Prints the content of an OmAudioBuffer IN LISP / LISTENER
(defun om-print-audiobuffer (buffer)
  (format nil "OmSpatAudioBuffer - numChannels = ~D - numSamples = ~D"
          (cffi:foreign-slot-value buffer 'omspat-audiobuffer_T 'numChannels)
          (cffi:foreign-slot-value buffer 'omspat-audiobuffer_T 'numSamples)))

(cffi:defcfun ("OmSpatResizeAudioBuffer" OmSpatResizeAudioBuffer) :boolean 
  (buffer (:pointer omspat-audiobuffer_T))
  (numChannels :unsigned-int) (numSamples :unsigned-long))

(cffi:defcfun ("OmSpatFreeAudioBuffer" OmSpatFreeAudioBuffer) :boolean 
  (buffer (:pointer omspat-audiobuffer_T)))


;;;================================================================================
;;; OSC
;;;================================================================================

(cffi:defcstruct OmSpatOscBundle (len :long) (ptr :pointer))
(cffi:defctype osc_bundle_T (:struct OmSpatOscBundle))

(cffi:defcfun ("OmSpatDebugOSCPacket" OmSpatDebugOSCPacket) :boolean (contents :pointer) (size :unsigned-long))
(cffi:defcfun ("OmSpatDebugOSCBundle" OmSpatDebugOSCBundle) :boolean (bundle (:pointer (:struct OmSpatOscBundle))))


;;;================================================================================
;;; COMPONENTS
;;;================================================================================

(cffi:defcfun ("OmSpatGetListOfComponents" OmSpatGetListOfComponents) :string)
(cffi:defcfun ("OmSpatCreateComponentWithType" OmSpatCreateComponentWithType) :pointer (type :string))
(cffi:defcfun ("OmSpatFreeComponent" OmSpatFreeComponent) :boolean (component :pointer))
(cffi:defcfun ("OmSpatGetComponentType" OmSpatGetComponentType) :string (component :pointer))
(cffi:defcfun ("OmSpatIsValidComponentType" OmSpatIsValidComponentType) :boolean (type :string))
(cffi:defcfun ("OmSpatIsDspComponent" OmSpatIsDspComponent) :boolean (component :pointer))
(cffi:defcfun ("OmSpatIsGuiComponent" OmSpatIsGuiComponent) :boolean (component :pointer))
(cffi:defcfun ("OmSpatGetCurrentStateAsOscBundle" OmSpatGetCurrentStateAsOscBundle) :pointer (component :pointer))
(cffi:defcfun ("OmSpatGetBundleFromGuiToDsp" OmSpatGetBundleFromGuiToDsp) :pointer (component :pointer))
(cffi:defcfun ("OmSpatProcessOscCommands" OmSpatProcessOscCommands) :boolean (component :pointer)(bundle :pointer))

; (OmSpatGetListOfComponents)
; (OmSpatCreateDSPComponentWithType "spat5.cascade~" 0 0)
;;;=======================================================
;;; DSP COMPONENTS
;;;=======================================================

(cffi:defctype spat_component_T :pointer)

(cffi:defcfun ("OmSpatCreateDspComponentWithType" OmSpatCreateDspComponentWithType) spat_component_T 
  (type :string)   
  (numInputs :unsigned-int) 
  (numOutputs :unsigned-int))

(cffi:defcfun ("OmSpatProcessAudio" OmSpatProcessAudio) :boolean 
  (obj :pointer)
  (out :pointer)
  (int :pointer)
  (numSamplesToProcess :unsigned-long))


;;;================================================================================
;;; GUI COMPONENTS
;;;================================================================================

(cffi:defcfun ("OmSpatInstallComponentInNSView" OmSpatInstallComponentInNSView) :boolean (component :pointer) (view :pointer))
(cffi:defcfun ("OmSpatRemoveFromNSView" OmSpatRemoveFromNSView) :boolean (component :pointer))
(cffi:defcfun ("OmSpatRegisterOscCallback" OmSpatRegisterOscCallback) :boolean (component :pointer) (callback :pointer))

;;; to call with OmSpatInstallComponentInNSView
(defun spat-get-view-pointer (view)
  #+macosx(objc::objc-object-pointer (capi-internals::representation view))
  #+windows(win32::hwnd (capi-internals::representation view)))

(defun spat-component-register-callback (component)
  (spat::OmSpatRegisterOscCallback component (cffi::get-callback 'spat-component-callback)))

(cffi::defcallback spat-component-callback :void ((component :pointer) (bundle :pointer))
  (handler-bind ((error #'(lambda (e) (print (format nil "ERROR IN SPAT CALLBACK: ~% ~A" e)))))
    (spat-component-handle-callback component bundle)))

;;; to be redefined
(defun spat-component-handle-callback (component-ptr bundle-ptr) 
  (declare (ignore component-ptr bundle-ptr))
  (print "Callback undefined"))


