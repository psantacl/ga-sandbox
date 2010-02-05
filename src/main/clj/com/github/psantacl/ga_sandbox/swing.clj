(ns com.github.psantacl.ga-sandbox.swing
  (:require [com.github.psantacl.ga-sandbox.einstein-main :as einstein]))

(comment

(def *icons*
  {
   :colors {
            :blue   	  "images/house-icons/house-blue.png"
            :green  	  "images/house-icons/house-green.png"
            :red    	  "images/house-icons/house-red.png"
            :white  	  "images/house-icons/house-white.png"
            :yellow 	  "images/house-icons/house-yellow.png"
            }
   :nationalities {
                   :Dane        "images/flag-icons/denmark.png"
                   :German      "images/flag-icons/germany.png"
                   :Norwegian   "images/flag-icons/norway.png"
                   :Swede       "images/flag-icons/sweden.png"
                   :Brit        "images/flag-icons/uk.png"
                   }
   :drinks {
            :beer   	  "images/drink-icons/beer-button.png"
            :coffee 	  "images/drink-icons/coffee-button.png"
            :milk   	  "images/drink-icons/milk-button.png"
            :tea    	  "images/drink-icons/tea-button.png"
            :water  	  "images/drink-icons/water-button.png"
            }
   :tobacco {
             :blend       "images/cigar-icons/blend.png"
	     :prince      "images/cigar-icons/prince.png"
	     :blue-master "images/cigar-icons/blue-master.png"
	     :dunhill     "images/cigar-icons/dunhill.png"
	     :pall-mall   "images/cigar-icons/pall-mall.png"
             }
   :pets {
          :cat    	  "images/pet-icons/cat-button.png"
          :dog    	  "images/pet-icons/dog-button.png"
          :fish   	  "images/pet-icons/fish-button.png"
          :horse  	  "images/pet-icons/horse-button.png"
          :bird   	  "images/pet-icons/parrot-button.png"
          }
  })

(def *icons-by-symbol*
     (loop [res {}
            [k & ks] (keys *icons*)]
       (if k
         (recur (merge res (*icons* k))
                ks)
         res)))

(comment

  (count (keys *icons-by-symbol*))
  ;; => 25

  )

(def *gui* (atom { :jf (javax.swing.JFrame.)}))
(def split-pane (javax.swing.JSplitPane. javax.swing.JSplitPane/VERTICAL_SPLIT))
(.setDividerLocation split-pane 300)
(.setContentPane (:jf @*gui*) split-pane)

(comment

  (:jf @*gui*)
  (.setVisible (:jf @*gui*) true)
)

(def model
     (proxy [javax.swing.table.AbstractTableModel]
         []
       (getValueAt [rowIdx colIdx]
                   (prn (format "getValueAt(rowIdx=%s; colIdx=%s) => Object" rowIdx colIdx))
                   "**object returned from getValueAt**")
       (getRowCount []
                    (prn (format "getRowCount() => int"))
                    5)
       (getColumnCount []
                       (prn (format "getColumnCount() => int"))
                       5)))


(def jt (javax.swing.JTable. model))

(defn image-cell-renderer []
  (proxy [javax.swing.table.DefaultTableCellRenderer]
      []
    (getTableCellRendererComponent
     [jtable value is-selected has-focus rowIdx colIdx]
     ;; TODO: need to finish porting / implementing this...
     (prn (format "getTableCellRendererComponent(jtable=%s; value=%s; is-selected=%s; has-focus=%s rowIdx=%s; colIdx=%s) => Component"))
     nil)
    ))


;; TODO: need to re-implement this using the above clojure proxy...
(defn new-cell-renderer [img]
  (let [R (image-cell-renderer)]
    ;; (.imagePath$ R img)
    R))

(comment

  (. (image-cell-renderer) imagePath "/foo/bar/qux.img")
  (bean (image-cell-renderer))

  (doseq [meth (com.github.kyleburton.sandbox.utils/fields-seq javax.swing.table.DefaultTableCellRenderer)]
    (prn meth))

  (doseq [meth (com.github.kyleburton.sandbox.utils/methods-seq javax.swing.table.DefaultTableCellRenderer)]
    (prn meth))
  )


(doseq [ii (range 4)]
  (.setCellRenderer (.getColumn (.getColumnModel jt) ii)
                    (new-cell-renderer "images/house-icons/house-blue.png")))

(def js (javax.swing.JScrollPane. jt))

(doseq [ii (range 5)]
    (.setMinWidth (.getColumn (.getColumnModel jt) ii) 128))
  (.setRowHeight jt 128)

(.add (.getContentPane (:jf @*gui*)) js)


(def lower-frame (javax.swing.JPanel. (java.awt.BorderLayout.)))
(.add (.getContentPane (:jf @*gui*)) lower-frame)
(.setLayout lower-frame (javax.swing.BoxLayout. lower-frame javax.swing.BoxLayout/Y_AXIS))

(def messages
      (javax.swing.JTextArea. "starting up..."))

(.add lower-frame messages java.awt.BorderLayout/CENTER)

(.setExtendedState (:jf @*gui*) (bit-or (.getExtendedState (:jf @*gui*)) java.awt.Frame/MAXIMIZED_BOTH))
(.pack (:jf @*gui*))

;; TODO: need to pull these out of a differnet structure (see the map at the top)
;; (doseq [ii (range 5)]
;;   (.setMatrixEntry model 0 ii (.nth *house-icons* ii)))

(.fireTableDataChanged model)

(def *gui-agent* (agent {:gui *gui* :best-genome nil})) ;'(random-genome)

(defn show-gui [agent]
  (prn (format "show-gui: agent=%s" agent))
  (.setVisible (:jf @(:gui agent)) true)
  agent)

;; *gui*
;; *gui-agent*
;; (.setVisible (:jf @(:gui @*gui-agent*)) true)

;; (prn (format "%s" @*gui-agent*))

(defn stop-gui [agent]
  ;; TODO: make the gui here and set it as the agent state...
  (.setVisible (:jf @(:gui agent)) false)
  agent)

(defn show-genome [agent genome]
  ;; update the table with the new genome here
  (assoc agent :best-genome genome))


(comment

  (send-off *gui-agent* show-gui)
  (send-off *gui-agent* show-gui (random-genome))
  (send-off *gui-agent* stop-gui)

  (def errs (agent-errors *gui-agent*))

  (.printStackTrace (first errs))

  (clear-agent-errors *gui-agent*)

  ;; some default icons for the grid

)

)
