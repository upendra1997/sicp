(ns sicp.pictureLang
  (:import [java.awt.image BufferedImage]
           [java.awt Graphics2D Color BasicStroke]
           [javax.imageio ImageIO]
           [java.io File]))

;; --- PRIMITIVE 1: Vector Abstraction ---
(defn make-vect [x y] {:x (double x) :y (double y)})

(def ^:dynamic ^Graphics2D *g2d* nil)

;; --- PRIMITIVE 2: Low-Level Screen Draw ---
(defn draw-line [v1 v2]
  (if-let [^Graphics2D g2d *g2d*]
    (.drawLine g2d
               (int (:x v1))
               (int (:y v1))
               (int (:x v2))
               (int (:y v2)))
    (throw (RuntimeException. "Cannot draw if graphics2D is not set"))))

#_(defn draw-img [path origin size]
  (if-let [^Graphics2D g2d *g2d*]
    (let [img (ImageIO/read (File. path))
          transform (java.awt.geom.AffineTransform.)] 
      (. transform scale 1 1) 
      (. transform translate 20 20)
      (prn (str "drawing " path " at " origin " with size " size))
      (.drawImage g2d
                  img
                  transform
                  nil))
    (throw (RuntimeException. "Cannot draw if graphics2D is not set"))))

;; --- PRIMITIVE 3: Export Painter directly to PNG ---
(defn save-painter-to-png
  "Renders a Painter function `(fn [g2d frame])` to an image file on disk.
   Perfect for SSH / headless environments."
  [painter filename width height]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        g2d (.createGraphics img)]
    (binding
     [*g2d* g2d]
      (try
        ;; 1. Fill background white
        (doto g2d
          (.setColor Color/WHITE)
          (.fillRect 0 0 width height)
          (.setColor Color/BLACK)
          (.setStroke (BasicStroke. 2.0)))
      
        ;; 2. Set up SICP frame coordinates (origin at bottom-left, y pointing UP)
        (let [padding 20.0
              origin (make-vect padding (- height padding))
              edge1  (make-vect (- width (* 2 padding)) 0)
              edge2  (make-vect 0 (- (* 2 padding) height))
              frame  {:origin origin :edge1 edge1 :edge2 edge2}]
      
          ;; 3. Execute painter logic onto image graphics context
          (painter frame))
      
        ;; 4. Save buffer to disk
        (ImageIO/write img "png" (File. filename))
        (println (str "Successfully saved image to: " filename))
      
        (finally
          (.dispose g2d))))))

(defn paint [painter & {:keys [file] :or {file "output.png"}}]
  (save-painter-to-png painter file 600 600))