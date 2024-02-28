;;; movi.el --- Moritz movie creator

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Marcos G Moritz <marcosmoritz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((hydra "0.15.0"))
;; Keywords: video, multimedia, ffmpeg, v4l2loopback
;; URL: https://mo-profile.com

;;; Commentary:
;; This is package to quickly create video

;; This is package to quickly create video using ffmpeg
;; from screen and webcam capture.
;; External dependencies:
;; - v4l2loopback
;; - ffmpeg
;; - slop

(cl-defstruct movi-input
  width
  height
  x-offset
  y-offset)

(cl-defstruct movi-output
  codec
  extension)

(cl-defstruct movi-webcam
  width
  alpha)

(defvar movi--process nil
  "variable to hold a recording process")

(defvar movi-dummy-camera--process nil
  "variable to hold dummy camera process")

(defvar movi--webcam-device "/dev/video0"
  "variable to webcam device")

(defvar movi-dummy-camera--device "/dev/video2"
  "dummy-camera output device")


(defvar movi-input-params (make-movi-input
                           :width 1920
                           :height 1080
                           :x-offset 0
                           :y-offset 0))

(defvar movi-output-params (make-movi-output
                            :codec "libx264"
                            :extension "mp4"))

(defvar movi-webcam-params (make-movi-webcam
                            :width 360
                            :alpha 0.8))

(defun movi-set-webcam-params ()
  (interactive)
  (setq movi-webcam-params
        (make-movi-webcam
         :width (read-number "Width: " 360)
         :alpha (read-number "Opacity: " 0.8))))

(defun movi-set-with-slop ()
  (interactive)
  (let ((screen-rectangle
         (mapcar 'string-to-number
                 (split-string (shell-command-to-string "slop -n -f '%g'")
                               "[x+]"))))
    (setq movi-input-params (make-movi-input
                             :width (let ((size (nth 0 screen-rectangle)))
                                      (if (cl-oddp size)
                                          (- size 1)
                                        size))
                             :height (let ((size (nth 1 screen-rectangle)))
                                       (if (cl-oddp size)
                                           (- size 1)
                                         size))
                             :x-offset (nth 2 screen-rectangle)
                             :y-offset (nth 3 screen-rectangle)))))

(defun movi-set-input (index)
  (setq movi-input-params (make-movi-input
                           :width (get-screen-width index)
                           :height (get-screen-height index)
                           :x-offset (* index (get-screen-width 0))
                           :y-offset 0))
  (message "%S" movi-input-params))

(defun movi-set-ouput (codec extension)
  (setq movi-output-params (make-movi-output
                            :codec codec
                            :extension extension))
  (message "%S" movi-output-params))

(defun movi-set-input-0 ()
  (interactive)
  (movi-set-input 0))

(defun movi-set-input-1 ()
  (interactive)
  (movi-set-input 1))

;; (movi-input-width movi-input-params)
;; (movi-input-height movi-input-params)
;; (movi-input-x-offset movi-input-params)
;; (movi-input-y-offset movi-input-params)

(defvar movi--selected-screen nil
  "Current selected screen to record")

(defun movi-select-screen (index)
  "Selects by screen number (1 or 2)"
  (interactive)
  (setq movi--selected-screen index))

(defun msg-me (process event)
  (princ
   (format "Process: %s had the event '%s'" process event)))

(defun movi-record (delay)
  "Start recording a video with audio and half of the screen"
  (interactive "p")
  (let ((filename "/tmp/video.mp4")
        (delay (if delay
                   delay
                 0))
        (process-name "movi-record"))
    (setq movi--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         (split-string (format "ffmpeg -y -f alsa -i pulse -f x11grab -r 25 -s 640x1000 -i :0.0+0,24 -acodec pcm_s16le -vcodec libx264 -threads 0 -ss %s %s"
                                               (number-to-string delay)
                                               filename)))))
    (set-process-sentinel movi--process 'msg-me)))

(defun movi-fullscreen-record (delay)
  "Start recording a video with audio and half of the screen"
  (interactive "p")
  (let ((filename "/tmp/video.mkv")
        (delay (if delay
                   delay
                 0))
        (process-name "movi-record"))
    (setq movi--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         (split-string (format "ffmpeg -y -f alsa -i pulse -f x11grab -r 25 -s %sx%s -i :0.0 -acodec pcm_s16le -vcodec libx264 -threads 0 -ss %s %s"
                                               1920 ;;(x-display-pixel-width)
                                               1080 ;;(x-display-pixel-height)
                                               delay
                                               filename)))))
    (set-process-sentinel movi--process 'msg-me)))

(defun movi-dummy-camera-record (delay)
  "Start recording a video with audio and half of the screen"
  (interactive "p")
  (let ((filename "/tmp/video.mkv")
        (delay (if delay
                   delay
                 0))
        (process-name "movi-record"))
    (setq movi--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         (split-string (format "ffmpeg -y -f alsa -i default -i %s -c:a flac -qscale 0 -ss %s %s"
                                               movi-dummy-camera--device
                                               delay
                                               filename)))))
    (set-process-sentinel movi--process 'msg-me)))


(defun movi-mute-record (delay)
  "starts a video recording with half of the screen"
  (interactive "p")
  (let ((filename "/tmp/video.mp4")
        (delay (if delay
                   delay
                 0))
        (process-name "movi-record"))
    (setq movi--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         (split-string (format "ffmpeg -y -r 30 -s 960x1056 -f x11grab -i :0.0 -vcodec libx264 -qscale 2 -ss %s %s"
                                               (number-to-string delay)
                                               filename)))))
    (set-process-sentinel movi--process 'msg-me)))

(defun movi-record-selection ()
  "Record selected screen"
  (interactive)
  (let* ((filename "/tmp/video.mp4")
         (process-name "movi-window-record")
         (command (split-string
                   (format "ffmpeg -y -f x11grab -s %sx%s -r 25 -i :0.0+%d,%d -vcodec libx264 %s"
                           (movi-input-width movi-input-params)
                           (movi-input-height movi-input-params)
                           (movi-input-x-offset movi-input-params)
                           (movi-input-y-offset movi-input-params)
                           filename))))
    (message (cl-reduce (lambda (a b) (format "%s %s" a b)) command))
    (setq movi--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         command)))
    (set-process-sentinel movi--process 'msg-me)))

(defun movi-stop ()
  "stops the video recording process"
  (interactive)
  (interrupt-process movi--process))


(defun movi-compress ()
  "Converts last saved video to the output format"
  (interactive)
  (let ((input-file "/tmp/video.mkv")
        (output-file (concat "/tmp/compressed." (movi-output-extension movi-output-params)))
        (process-name "movi-compress"))
    (setq movi--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         (split-string (format "ffmpeg -y -i %s -vcodec %s -crf 22 -threads 0 %s"
                                               input-file
                                               (movi-output-codec movi-output-params)
                                               output-file)))))
    (set-process-sentinel movi--process 'msg-me)))

(defun movi-sync (&optional delay)
  "Synchronize audio and video"
  (interactive)
  (let ((input-file "/tmp/compressed.mp4")
        (output-file (concat "/tmp/sync.mp4"))
        (delay (if delay
                   delay
                 "00:00.400"))
        (process-name "movi-sync"))
    (setq movi--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         (split-string (format "ffmpeg -y -i %s -itsoffset %s -i %s -vcodec copy -acodec copy -map 0:0 -map 1:1 %s"
                                               input-file
                                               delay
                                               input-file
                                               output-file)))))
    (set-process-sentinel movi--process 'msg-me)))

(defun movi-play ()
  "Plays the last saved video"
  (interactive)
  (let ((filename "/tmp/sync.mp4"))
    (async-shell-command (format "mplayer %s" filename))))

(defun movi-browse ()
  "Plays the last saved video"
  (interactive)
  (let ((filename "/tmp/video.mp4"))
    (async-shell-command (format "nautilus %s" filename))))

(defun movi-dummy-camera-start (&optional alpha)
  "Start capturing with a dummy camera that takes input from camera and the screen"
  (interactive)
  (let* ((process-name "movi-dummy-camera")
         (webcam-witdh (movi-webcam-width movi-webcam-params))
         (alpha (movi-webcam-alpha movi-webcam-params))
         (cam-alpha-options (if alpha
                                (format ",format=argb,geq=r='r(X,Y)':a='%f*alpha(X,Y)'" alpha)
                              "")))
    (setq movi-dummy-camera--process
          (apply 'start-process
                 (append `(,process-name
                           ,process-name)
                         (split-string (format "ffmpeg -f alsa -thread_queue_size 1024 -i default -f x11grab -s %sx%s -r 25 -i :0.0+%d,%d -f video4linux2 -i %s -filter_complex [2:v]scale=%d:-1%s[cam];[1:v][cam]overlay=W-w-8:H-h-8 -c:a flac -qscale 0 -vcodec rawvideo -pix_fmt yuv420p -threads 0 -f v4l2 %s"
                                               (movi-input-width movi-input-params)
                                               (movi-input-height movi-input-params)
                                               (movi-input-x-offset movi-input-params)
                                               (movi-input-y-offset movi-input-params)
                                               movi--webcam-device
                                               webcam-witdh
                                               cam-alpha-options
                                               movi-dummy-camera--device)))))
    (set-process-sentinel movi-dummy-camera--process 'msg-me)))

(defun movi-dummy-camera-stop ()
  "stops the video recording process"
  (interactive)
  (interrupt-process movi-dummy-camera--process))

(defun get-screen-sizes ()
  (split-string (shell-command-to-string "echo -n $(xrandr | grep \\* | cut -d\" \" -f4)" )))

(defun get-screen-size (index)
  (elt (get-screen-sizes) index))

(defun get-screen-width (index)
  (string-to-number (elt (split-string (get-screen-size index) "x") 0)))

(defun get-screen-height (index)
  (string-to-number (elt (split-string (get-screen-size index) "x") 1)))

(require 'hydra)
(defhydra hydra-movi ()
  "Moritz video"
  ("1" movi-set-input-0 "Select first monitor")
  ("2" movi-set-input-1 "Select second monitor")
  ("S" movi-set-with-slop "Select window to record")
  ("h" movi-record "Record half-screen")
  ("r" movi-record-selection "Record selected window")
  ("f" movi-fullscreen-record "Record fullscreen video")
  ("x" movi-dummy-camera-record "Record dummy camera")
  ("c" movi-compress "Compress")
  ("m" movi-mute-record "Record half-screen mute video")
  ("p" movi-play "Play last compressed video")
  ("s" movi-stop "Stop recording video")
  ("l" movi-sync "Synchronize video and audio")
  ("b" movi-browse "Open in nautilus")
  ("d" movi-dummy-camera-start "Start dummy camera")
  ("k" movi-dummy-camera-stop "Stop dummy camera")
  ("q" nil "quit" :color blue))

(provide 'movi)
