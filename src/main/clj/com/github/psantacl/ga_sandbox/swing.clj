(ns com.github.psantacl.ga-sandbox.swing)

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


(def jf (javax.swing.JFrame.))
(def split-pane (javax.swing.JSplitPane. javax.swing.JSplitPane/VERTICAL_SPLIT))
(.setDividerLocation split-pane 300)
(.setContentPane jf split-pane)

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

(.add (.getContentPane jf) js)


(def lower-frame (javax.swing.JPanel. (java.awt.BorderLayout.)))
(.add (.getContentPane jf) lower-frame)
(.setLayout lower-frame (javax.swing.BoxLayout. lower-frame javax.swing.BoxLayout/Y_AXIS))

(def messages
      (javax.swing.JTextArea. "starting up..."))

(.add lower-frame messages java.awt.BorderLayout/CENTER)

(.setExtendedState jf (bit-or (.getExtendedState jf) java.awt.Frame/MAXIMIZED_BOTH))
(.pack jf)

;; TODO: need to pull these out of a differnet structure (see the map at the top)
;; (doseq [ii (range 5)]
;;   (.setMatrixEntry model 0 ii (.nth *house-icons* ii)))

(.fireTableDataChanged model)

(.setVisible jf true)


(comment



  ;; some default icons for the grid

)

