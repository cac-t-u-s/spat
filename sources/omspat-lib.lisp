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

;===========================
; FOREIGN LIB LOADER
; @author: J. Bresson
;===========================

(in-package :spat)

(om::add-lib-alias "om-spat" "om-spat5")

(om::require-om-package "sound")

(om::require-library "odot")


;;; if the library is included in teh lib release, taht would be here:
;;; (merge-pathnames "lib/mac/OmSpat.framework/OmSpat" (om::mypathname (om::find-library "om-spat5")))

(defun default-spatlib-path ()
  (merge-pathnames "Documents/Max 8/Packages/spat5/media/omspat/OmSpat.framework/OmSpat"
                   (om::om-user-home)
                   ))

(defparameter *spat* nil)

(defun load-spat-lib ()
  (let ((sharedlibpath (om::get-pref-value :externals :spat5lib-path)))

    (if (and sharedlibpath (probe-file sharedlibpath))

        (let ((lib (om-fi::om-load-foreign-library
                    "LIBOMSPAT"
                    `((:macosx ,sharedlibpath)
                      (:windows ,(om-fi::om-foreign-library-pathname "omspat.dll"))
                      (t (:default "omspat"))))))
          
          (if lib
          ;(when *load-pathname* ;; we are loading this...
          ;  (compile&load (merge-pathnames "omspat-api" *load-pathname*)))
              (progn 
                (setf *spat* t)
                (spat::OmSpatInitialize)
                (spat::OmSpatSetVerbose t)
                (om::om-print (spat::OmSpatGetVersion) "SPAT")
                (if (spat::OmSpatIsInitialized)
                    (push :spat *features*)
                  (om-print "Initialization Error." "SPAT"))
                )
            (om::om-message-dialog "Warning: Spat shared library could not be loaded.")
            ))

      (om::om-beep-msg "Library OMSpat not found:~A. See Preferences/Externals" sharedlibpath))

    ))

;;(probe-file "C:\\Program Files (x86)\\LispWorks\\omspat.dll")
;; (fli:register-module :spat :connection-style :immediate :real-name "C:\\Program Files (x86)\\LispWorks\\omspat.dll")

(om::add-preference-section :externals "OM-Spat" nil '(:spat5lib-path))

;; will load now and each time the preference is modified...
(om::add-preference  :externals :spat5lib-path "OMSpat dynamic library path" :file (default-spatlib-path) nil 'load-spat-lib)


(load-spat-lib)



