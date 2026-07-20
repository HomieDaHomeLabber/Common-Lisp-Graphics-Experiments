;;; ============================================================================Debugged by Claude 7/7/2026
;;; 1. GLOBALS, CONSTANTS, AND STRUCTURES
;;; ============================================================================

(defstruct canvas-obj
    width
      height
        data)

(defparameter *background-color* #(0 0 0))

(defstruct sphere
    center
      radius
        color
          (specular 10 :type fixnum)
            (reflectivity 0.0 :type float))

(defparameter *spheres* nil)
(defparameter *light-position* #(0.0 2.0 2.0))
(defparameter *light-intensity* 1.0)


;;; ============================================================================
;;; 2. VECTOR MATH
;;; ============================================================================

(defun vec-subtract (v1 v2)
    (vector (- (aref v1 0) (aref v2 0))
                      (- (aref v1 1) (aref v2 1))
                                (- (aref v1 2) (aref v2 2))))

(defun vec-add (v1 v2)
    (vector (+ (aref v1 0) (aref v2 0))
                      (+ (aref v1 1) (aref v2 1))
                                (+ (aref v1 2) (aref v2 2))))

(defun vec-scale (v scalar)
    (vector (* (aref v 0) scalar)
                      (* (aref v 1) scalar)
                                (* (aref v 2) scalar)))

(defun vec-dot (v1 v2)
    (+ (* (aref v1 0) (aref v2 0))
            (* (aref v1 1) (aref v2 1))
                 (* (aref v1 2) (aref v2 2))))

(defun vec-length (v)
    (sqrt (vec-dot v v)))

(defun vec-normalize (v)
    (let ((len (vec-length v)))
          (if (< len 0.0001)
                      v
                              (vec-scale v (/ 1.0 len)))))


;;; ============================================================================
;;; 3. LIGHTING
;;; ============================================================================

(defun reflect-ray (ray normal)
    "Reflect a ray direction about a surface normal. R = 2(N·L)N - L"
    (vec-subtract (vec-scale normal (* 2.0 (vec-dot normal ray))) ray))

(defun in-shadow-p (point light-dir)
    "Returns T if any sphere blocks the path from POINT to the light.
   t-min 0.001 prevents self-intersection; t-max is the light distance."
    (let ((light-dist (vec-length (vec-subtract *light-position* point))))
          (multiple-value-bind (shadow-t shadow-sphere)
                      (find-closest-intersection point light-dir 0.001 light-dist)
                  (declare (ignore shadow-t))
                  (not (null shadow-sphere)))))

(defun compute-lighting (point normal view-dir specular-exp)
    "Compute ambient + diffuse + specular (Phong) lighting with shadows.
   view-dir is the direction FROM the surface TOWARD the camera (negated ray)."
    (let* ((light-dir (vec-normalize (vec-subtract *light-position* point)))
                    (n-dot-l   (max 0.0 (vec-dot normal light-dir)))
                             (ambient   0.2))
          (if (in-shadow-p point light-dir)
                      ;; In shadow: ambient only, no diffuse or specular
                      (* *light-intensity* ambient)
                              (let* ((diffuse  (* 0.8 n-dot-l))
                              (specular (if (and (> specular-exp 0) (> n-dot-l 0.0))
                              (let* ((reflect (reflect-ray light-dir normal))
                              (r-dot-v (min 1.0 (max 0.0 (vec-dot reflect view-dir)))))
                              (* (expt r-dot-v specular-exp) 0.5))
                              0.0)))
                              (* *light-intensity* (+ ambient diffuse specular))))))


;;; ============================================================================
;;; 4. CORE RAY TRACING
;;; ============================================================================

(defun intersect-ray-sphere (origin direction sphere)
    (let* ((r (sphere-radius sphere))
               (center (sphere-center sphere))
                (co (vec-subtract origin center))
                (a (vec-dot direction direction))
                (b (* 2.0 (vec-dot co direction)))
                (c (- (vec-dot co co) (* r r)))
                (discriminant (- (* b b) (* 4.0 a c))))

          (if (< discriminant 0.0)
                      (values most-positive-single-float most-positive-single-float)
                              (let ((sqrt-disc (sqrt discriminant)))
                                          ;; FIX: t1 is the smaller root (-b - sqrt) / 2a,
                                          ;;      t2 is the larger root  (-b + sqrt) / 2a.
                                          ;; Grok had these swapped, causing back-face hits to be preferred.
                                          (values (/ (- (- b) sqrt-disc) (* 2.0 a))
                                                                    (/ (+ (- b) sqrt-disc) (* 2.0 a)))))))

(defun find-closest-intersection (origin direction t-min t-max)
    (let ((closest-t most-positive-single-float)
                  (closest-sphere nil))

          (dolist (sphere *spheres*)
                  (multiple-value-bind (t1 t2) (intersect-ray-sphere origin direction sphere)
                            (when (and (<= t-min t1 t-max) (< t1 closest-t))
                                        (setf closest-t t1)
                                        (setf closest-sphere sphere))
                            (when (and (<= t-min t2 t-max) (< t2 closest-t))
                                        (setf closest-t t2)
                                        (setf closest-sphere sphere))))

          (values closest-t closest-sphere)))

(defun trace-ray (origin direction t-min t-max depth)
    "Traces a ray recursively up to DEPTH bounces.
   Returns a lit, reflection-blended, clamped RGB color vector."
    (multiple-value-bind (closest-t closest-sphere)
              (find-closest-intersection origin direction t-min t-max)

          (if (null closest-sphere)
                      *background-color*
                              (let* ((point (vec-add origin (vec-scale direction closest-t)))
                                    (normal (vec-normalize (vec-subtract point (sphere-center closest-sphere))))
                                                                   ;; view-dir points FROM surface TOWARD camera (negate incoming ray)
                                    (view-dir   (vec-scale direction -1.0))
                                    (light-amt  (min 1.0 (compute-lighting point normal view-dir
                                    (sphere-specular closest-sphere))))
                                    (base-color (sphere-color closest-sphere))
                                    (lit-color  (map 'vector (lambda (x) (min 255 (max 0 (round (* x light-amt)))))
                                     base-color))
                                     (reflect    (sphere-reflectivity closest-sphere)))

                                          ;; Recursive mirror reflection — only if surface is reflective and
                                          ;; we have bounces remaining. This is the recurso en recursividad moment.
                                          (if (or (<= depth 0) (<= reflect 0.0))
                                            lit-color
                                          (let* ((reflect-dir   (reflect-ray (vec-scale direction -1.0) normal))
                                          (reflect-color (trace-ray point reflect-dir 0.001  most-positive-single-float
                                                                               (1- depth)))                                                                                                                         
                                          (local-weight  (- 1.0 reflect))
                                          (blended       (map 'vector
                                                              (lambda (local refl)
                                                                (round (+ (* local local-weight)
                                                                          (* refl reflect))))
                                                              lit-color reflect-color)))
                                          blended))))))


;;; ============================================================================
;;; 5. RENDERING
;;; ============================================================================

(defun put-pixel (canvas x y color)
    (with-slots (width height data) canvas
          (let ((array-x (+ x (truncate width 2)))
                          ;; FIX: Negate y so that canvas +Y is up, matching Gambetta's
                          ;;      coordinate system, while PPM row 0 is at the top.
                          (array-y (- (truncate height 2) y)))
                  (when (and (<= 0 array-x (1- width))
                                              (<= 0 array-y (1- height)))
                            (setf (aref data array-y array-x 0) (aref color 0))
                            (setf (aref data array-y array-x 1) (aref color 1))
                            (setf (aref data array-y array-x 2) (aref color 2))))))

(defun canvas-to-viewport (x y)
    "Maps a canvas pixel to a viewport direction vector. Viewport is 1x1 at z=1."
    (vector (* x (/ 1.0 300.0))
                      (* y (/ 1.0 300.0))
                                1.0))

(defun render-scene (canvas)
    (let* ((cw     (canvas-obj-width canvas))
                    (ch     (canvas-obj-height canvas))
                             (half-w (truncate cw 2))
                                      (half-h (truncate ch 2)))

          (loop for x from (- half-w) to (1- half-w) do
                      (loop for y from (- half-h) to (1- half-h) do
                                    (let* ((d     (canvas-to-viewport x y))
                                                          (color (trace-ray #(0.0 0.0 0.0) d 1.0 most-positive-single-float 3)))
                                                (put-pixel canvas x y color))))))

(defun export-canvas-to-ppm (canvas filename)
    "Exports the canvas to a binary PPM (P6) file."
    (with-slots (width height data) canvas
          (with-open-file (stream filename
            :direction :output
            :if-exists :supersede
            :element-type '(unsigned-byte 8))
                  (labels ((write-str (str)
                                              (loop for char across str do (write-byte (char-code char) stream))))
                            (write-str (format nil "P6~%~D ~D~%255~%" width height)))

                  (loop for y from 0 below height do
                                (loop for x from 0 below width do
                                                (write-byte (aref data y x 0) stream)
                                                (write-byte (aref data y x 1) stream)
                                                (write-byte (aref data y x 2) stream)))
                  nil)))

(defun output-ppm-to-png (ppm-filename png-filename)
    "Converts PPM to PNG using ImageMagick if present, fails gracefully."
    (let ((check-process (sb-ext:run-program "/usr/bin/which"                                                                                        '("convert") :search t :wait t)))
          ;; Check if the command 'which' returned 0 (success)
          (if (= (sb-ext:process-exit-code check-process) 0)
                      (progn (sb-ext:run-program "convert"
                              (list ppm-filename png-filename) :search t  :wait t)
                                            (format t "Successfully converted ~a to ~a~%" ppm-filename png-filename))
                              (format t "ImageMagick not found. Skipping conversion. PPM file: ~a~%" ppm-filename))))

(defun get-png (ppm-filename)
    "Takes a .ppm filename string or pathname and returns a string with a .png extension."
    (namestring (make-pathname :type "png" :defaults ppm-filename)))








;;; ============================================================================
;;; 6. SCENE SETUP AND ENTRY POINT
;;; ============================================================================
(defun main (&key (width 600) (height 600) (filename nil) (spheres nil))
    ;; 1. Generate a timestamped filename if none is provided
    (let ((final-filename
          (or filename
          (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (get-universal-time))
          (declare (ignore sec)) ; <--- Add this line!
          (format nil "output-~4d-~2,'0d-~2,'0d-~2,'0d~2,'0d.ppm"
           year month day hour min)))))

          ;; 2. Set up the spheres
          (let ((scene-spheres (or spheres
          (list (make-sphere :center #(0.0 -1.0 3.0)  :radius 1.0  :color #(255 0 0)   :specular 500 :reflectivity 0.2)
          (make-sphere :center #(2.0  0.0 4.0)  :radius 1.0  :color #(0 0 255)   :specular 500 :reflectivity 0.3)
          (make-sphere :center #(-2.0 0.0 4.0)  :radius 1.0  :color #(0 255 0)   :specular 10 :reflectivity 0.4)
          (make-sphere :center #(0.0 -5001.0 0.0) :radius 5000.0 :color #(255 255 0) :specular 1000 :reflectivity 0.5)))))

                  (setf *spheres* scene-spheres)

                  ;; 3. Create canvas and render
                  (let ((canvas (make-canvas-obj
                   :width  width :height height :data   (make-array (list height width 3) :element-type '(unsigned-byte 8)
                   :initial-element 0))))
                            (format t "Rendering to ~a...~%" final-filename)
                            (render-scene canvas)
                            (export-canvas-to-ppm canvas final-filename)
                            (format t "Done! Written to ~a~%" final-filename)
                            ;; 4. Best-effort PNG conversion — see section 7 at the end of the file.
                              (output-ppm-to-png final-filename (get-png final-filename))))))
                    

;;--------------------------------------------------------------------
;;Cooler Shapes than spheres to refactor
;;round cube
(defun intersect-ray-rounded-cube (origin direction center half-size roundness)
    "Intersect a ray with a rounded cube via sphere tracing.
   half-size controls overall size, roundness controls corner radius.
   Uses SDF raymarching since no analytic closed form exists."
    (flet ((sdf (p)
                        ;; Signed distance to a rounded box
                        (let* ((q (vector (- (abs (- (aref p 0) (aref center 0))) half-size)
                              (- (abs (- (aref p 1) (aref center 1))) half-size)
                              (- (abs (- (aref p 2) (aref center 2))) half-size)))
                                                 (qx (aref q 0)) (qy (aref q 1)) (qz (aref q 2))
                                                                   ;; length(max(q,0)) + min(max(qx,qy,qz),0) - roundness
                             (outer (sqrt (+ (expt (max 0.0 qx) 2)
                             (expt (max 0.0 qy) 2)
                             (expt (max 0.0 qz) 2))))
                               (inner (min (max qx qy qz) 0.0)))
                                       (- (+ outer inner) roundness))))
          (raymarch-sdf #'sdf origin direction 0.001 100.0)))

(defun raymarch-sdf (sdf origin direction t-min t-max)
    "Generic SDF raymarcher. Sphere traces along the ray until it hits
   the surface (distance < epsilon) or exits the scene (t > t-max).
   Returns (values t-hit hit-p) or (values nil nil) on miss."
    (let ((current-t t-min)
                  (epsilon 0.001))
          (loop
                  (when (> current-t t-max)
                            (return (values most-positive-single-float nil)))
                  (let* ((point (vec-add origin (vec-scale direction current-t)))
                                      (dist  (funcall sdf point)))
                            (when (< (abs dist) epsilon)
                                        (return (values current-t point)))
                            (setf current-t (+ current-t (abs dist)))))))

(defun sdf-normal (sdf point)
    "Estimate surface normal via central differences.
   Works for any SDF without needing an analytic gradient."
    (let ((eps 0.001))
          (vec-normalize
                 (vector (- (funcall sdf (vector (+ (aref point 0) eps)
                 (aref point 1)
                 (aref point 2)))
                 (funcall sdf (vector (- (aref point 0) eps)
                 (aref point 1)
                 (aref point 2))))
                         (- (funcall sdf (vector (aref point 0)
                         (+ (aref point 1) eps)
                         (aref point 2)))
                 (funcall sdf (vector (aref point 0)
                                      (- (aref point 1) eps)
                                      (aref point 2))))
                         (- (funcall sdf (vector (aref point 0)
                                                 (aref point 1)
                                                 (+ (aref point 2) eps)))
                            (funcall sdf (vector (aref point 0)
                                                 (aref point 1)
                                                 (- (aref point 2) eps))))))))
;;Torus
(defun sdf-torus (point center major-r minor-r)
    "Signed distance to a torus.
   major-r = distance from center to tube center (the donut radius).
   minor-r = radius of the tube itself (the hole tightness).
   Torus lies in the XZ plane centered at CENTER."
    (let* ((p  (vec-subtract point center))
                    (px (aref p 0))
                             (py (aref p 1))
                                      (pz (aref p 2))
                                               ;; Project onto XZ plane, measure distance to the ring
                                               (ring-dist (- (sqrt (+ (* px px) (* pz pz))) major-r)))
          ;; Distance to the tube surface
          (- (sqrt (+ (* ring-dist ring-dist) (* py py))) minor-r)))

(defun intersect-ray-torus (origin direction center major-r minor-r)
    "Intersect a ray with a torus via SDF raymarching."
    (flet ((sdf (p) (sdf-torus p center major-r minor-r)))
          (raymarch-sdf #'sdf origin direction 0.001 100.0)))

(defun torus-normal (point center major-r)
    "Analytic normal for a torus — faster than central differences."
    (let* ((p  (vec-subtract point center))
                    (px (aref p 0))
                    (py (aref p 1))
                    (pz (aref p 2))
                    (ring-dist (sqrt (+ (* px px) (* pz pz))))
                                                        ;; Closest point on the ring
                    (cx (* (/ px ring-dist) major-r))
                    (cz (* (/ pz ring-dist) major-r)))
          (vec-normalize (vector (- px cx) py (- pz cz)))))
;;mandel bulb ADD WARNINGS AND USER WALKTHROUGH
(defparameter *mandelbulb-power* 8.0
    "The bulb exponent. 8 is the classic. Try 3-12 for different shapes.")

(defparameter *mandelbulb-iterations* 10
    "More iterations = more detail but slower. 6-15 is practical range.")

(defun sdf-mandelbulb (point)
    "Signed distance estimate to the Mandelbulb fractal.
   Based on Inigo Quilez's distance estimator formula.
   Not an exact SDF — an estimate — so use smaller epsilon in raymarcher."
    (let* ((n   *mandelbulb-power*)
                    (w   (vec-scale point 1.0))  ; copy
                             (m   (vec-dot w w))          ; |w|^2
                                      (dz  1.0))                   ; derivative accumulator

          (dotimes (i *mandelbulb-iterations*)
                  ;; Derivative: dz = n * |w|^(n-1) * dz + 1
                  (setf dz (+ (* n (expt (sqrt m) (1- n)) dz) 1.0))

                  ;; Convert to spherical coordinates
                  (let* ((wx (aref w 0)) (wy (aref w 1)) (wz (aref w 2))
                  (r     (sqrt m))
                                         (theta (* n (atan (sqrt (+ (* wx wx) (* wz wz))) wy)))
                                        (phi   (* n (atan wz wx)))
                                         (rn    (expt r n)))

                            ;; Rotate and scale — the mandelbulb iteration
                            (setf w (vector (+ (* rn (sin theta) (cos phi)) (aref point 0))
                            (+ (* rn (cos theta))            (aref point 1))
                                            (+ (* rn (sin theta) (sin phi))  (aref point 2))))
                            (setf m (vec-dot w w)))

                  ;; Escape condition — outside the bulb
                  (when (> m 256.0)
                            (return-from sdf-mandelbulb
                                        ;; Distance estimator: 0.5 * log(|w|) * |w| / dz
                                        (* 0.5 (log m) (/ (sqrt m) dz)))))
          ;; Inside the bulb
          0.0))

(defun intersect-ray-mandelbulb (origin direction)
    "Intersect a ray with the Mandelbulb.
   Uses smaller epsilon because the SDF is an estimate not exact."
    ;; Tighter epsilon for fractal detail, shorter max distance, smaller step
    ;; multiplier (0.5x) since the DE formula tends to overstep near detail.
    (let ((current-t 0.001)
                  (epsilon   0.0005)
                          (t-max     10.0))
          (loop
                   (when (> current-t t-max)
                              (return (values most-positive-single-float nil)))
                   (let* ((point (vec-add origin (vec-scale direction current-t)))
                                        (dist  (sdf-mandelbulb point)))
                              (when (< (abs dist) epsilon)
                                           (return (values current-t point)))
                              (setf current-t (+ current-t (* 0.5 (abs dist))))))))
