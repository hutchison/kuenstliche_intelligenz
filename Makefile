.PHONY: clean

clean:
	rm -f *.aux *.log *.fls *.out *.fdb_latexmk

loesung01.pdf-live: loesung01.latex
	latexmk -pdf -pvc -xelatex -use-make $<

loesung01.pdf: loesung01.latex
	latexmk -pdf -xelatex -use-make $<

zusammenfassung.pdf-live: zusammenfassung.latex
	latexmk -pdf -pvc -xelatex -use-make $<

zusammenfassung.pdf: zusammenfassung.latex
	latexmk -pdf -xelatex -use-make $<

klausur_ss17.pdf: klausur_ss17.latex
	latexmk -pdf -xelatex $<

klausur_ss17.pdf-live: klausur_ss17.latex
	latexmk -pdf -pvc -xelatex $<
