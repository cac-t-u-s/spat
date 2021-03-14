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
; SPAT-OBJECT OMClass definitions
; @author: J. Bresson
;===========================



(in-package :om)


(defclass! spat-dsp (spat-object data-stream)
  ((audio-in :accessor audio-in :initform nil :initarg :audio-in :documentation "audio source")
   (dsp-type :initarg :dsp-type :accessor dsp-type :initform "spat5.filterdesign")
   (controls :initarg :controls :accessor controls :initform nil :documentation "list of timed OSC-bundles"))
  (:default-initargs :default-frame-type 'osc-bundle :action 'render-audio)
  (:documentation "A Spat sound processor.

Controls the transformation of a sound source using OSC bundles.

Offline rendering with SPAT-SYNTH."))


(defclass! spat-scene (spat-object time-sequence)
  ((audio-in :accessor audio-in :initform nil :initarg :audio-in :documentation "audio source file(s)") ;;; repeated slot to make it appear on the box
   (trajectories :accessor trajectories :initform nil :initarg :trajectories :documentation "source trajectories") ;list of 3DC
   (speakers :accessor speakers :initform '((-1 1 0) (1 1 0)) :initarg :speakers :documentation "speaker positions")    ;  (1 -1 0) (-1 -1 0)
   (controls :initarg :controls :accessor controls :initform nil :type list :documentation "list of timed OSC-bundles")
   (panning-type :accessor panning-type :initform "angular")
   (reverb :accessor reverb :initform nil)
   )
  (:documentation "A Spat spatialization processor.

Controls the spatialization of a set of sound sources using 3D-trajectories and other controllers formatted as OSC bundles.

Offline rendering with SPAT-SYNTH."))
