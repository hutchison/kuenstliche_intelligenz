.PHONY: clean

clean:
	rm -f *.aux *.log *.fls *.out *.fdb_latexmk

loesung01.pdf-live: loesung01.latex
	latexmk -pdf -pvc -xelatex -use-make $<

loesung01.pdf: loesung01.latex
	latexmk -pdf -xelatex -use-make $<
