(in-package :cl-user)

(defpackage :spat)

(compile&load (merge-pathnames "omspat" *load-pathname*))

(push :spat *features*)

(defun load-spat-lib ()
  (let ((libpath (merge-pathnames 
                  "lib/mac/OmSpat.framework/OmSpat" 
                  (om::mypathname (om::find-library "om-spat")))))
    (when (om-fi::om-load-foreign-library
           "LIBOMSPAT"
           `((:macosx ,libpath)
             (:windows ,(om-fi::om-foreign-library-pathname "omspat.dll"))
             (t (:default "omspat")))))
    (spat::OmSpatInitialize)
    (spat::OmSpatSetVerbose nil)
    ))

;;(probe-file "C:\\Program Files (x86)\\LispWorks\\omspat.dll")
;; (fli:register-module :spat :connection-style :immediate :real-name "C:\\Program Files (x86)\\LispWorks\\omspat.dll")

;; load now
(load-spat-lib)

(print (spat::OmSpatGetVersion))

;; load at OM startup
;; #+macosx(om-fi::add-foreign-loader 'load-spat-lib)

