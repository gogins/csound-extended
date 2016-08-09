(in-package :cm)

(defparameter dyads5-graph
  (new graph of 
       `(((1 2) :id ab :to ,(new random of '(cd de ce)))
	 ((1 3) :id ac :to ,(new random of '(de be bd)))
	 ((1 4) :id ad :to ,(new random of '(be bc ce)))
	 ((1 5) :id ae :to ,(new random of '(cd bd bc)))
	 ((2 3) :id bc :to ,(new random of '(de ae ad)))
	 ((2 4) :id bd :to ,(new random of '(ce ae ac)))
	 ((2 5) :id be :to ,(new random of '(cd ad ac)))
	 ((3 4) :id cd :to ,(new random of '(ab ae be)))
	 ((3 5) :id ce :to ,(new random of '(ab bd ad)))
	 ((4 5) :id de :to ,(new random of '(bc de ab))))))

; (next dyads5-graph 10)

(defparameter dyads6-graph
  (new graph of 
       `(((1 2) :id ab :to ,(new random of '(cd ef df cf ce de)))
	 ((1 3) :id ac :to ,(new random of '(de ef bf df bd be)))
	 ((1 4) :id ad :to ,(new random of '(ef bf bc be ce cf)))
	 ((1 5) :id ae :to ,(new random of '(bc bf bd cd df cf)))
	 ((1 6) :id af :to ,(new random of '(de ce bc be bd cd)))
	 ((2 3) :id bc :to ,(new random of '(de ef af ae df ad)))
	 ((2 4) :id bd :to ,(new random of '(ef af ac ae ce cf)))
	 ((2 5) :id be :to ,(new random of '(af ac ad cd cf df)))
	 ((2 6) :id bf :to ,(new random of '(de ce ac ae ad cd)))
	 ((3 4) :id cd :to ,(new random of '(ef ab af bf ae be)))
	 ((3 5) :id ce :to ,(new random of '(af bf ab bd df ad)))
	 ((3 6) :id cf :to ,(new random of '(ab bd ad be de ae)))
	 ((4 5) :id de :to ,(new random of '(bc ac af bf ab cf)))
	 ((4 6) :id df :to ,(new random of '(ab ac bc ce ae be)))
	 ((5 6) :id ef :to ,(new random of '(cd bd ab ad ac bc))))))


(defparameter dyads7-graph
  (new graph of 
       `(((1 2) :id ab :to ,(new random of '(cd fg eg dg ce cg de df ef cf)))
	 ((1 3) :id ac :to ,(new random of '(de fg bg eg bd bf dg be df ef)))
	 ((1 4) :id ad :to ,(new random of '(ef bg bc bf ce cg be cf eg fg)))
	 ((1 5) :id ae :to ,(new random of '(fg bc bd cd cg df cf dg bg bf)))
	 ((1 6) :id af :to ,(new random of '(bc bd bg cd de ce eg dg be cg)))
	 ((1 7) :id ag :to ,(new random of '(ef df bc cf bd bf cd ce de be)))
	 ((2 3) :id bc :to ,(new random of '(de ag fg af eg ae ad df dg ef)))
	 ((2 4) :id bd :to ,(new random of '(ef ag ac af ce cg ae cf eg fg)))
	 ((2 5) :id be :to ,(new random of '(fg ac cd cg ad df cf dg af ag)))
	 ((2 6) :id bf :to ,(new random of '(ag ac ad cd ce de eg dg ae cg)))
	 ((2 7) :id bg :to ,(new random of '(ef df ac cf ad cd af ce de ae)))
	 ((3 4) :id cd :to ,(new random of '(ab ef ag bg af bf be eg ae fg)))
	 ((3 5) :id ce :to ,(new random of '(fg ab bd bg df ad ag bf dg af)))
	 ((3 6) :id cf :to ,(new random of '(ag bg bd de ad be eg dg ae ab)))
	 ((3 7) :id cg :to ,(new random of '(ab bd ad be de ae df ef af bf)))
	 ((4 5) :id de :to ,(new random of '(bc ac fg ab bg cg cf af ag bf)))
	 ((4 6) :id df :to ,(new random of '(ag bg bc ce ac eg be ab cg ae)))
	 ((4 7) :id dg :to ,(new random of '(ab ac bc ce ef be cf af ae bf)))
	 ((5 6) :id ef :to ,(new random of '(cd bd ag ad bg bc ac dg ab cg)))
	 ((5 7) :id eg :to ,(new random of '(ab ac bc cd df bd af cf ad bf)))
	 ((6 7) :id fg :to ,(new random of '(de ce ab be ac ae bc cd bd ad))))))
