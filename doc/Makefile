TARGET=report

all: pdf

report.pdf: $(wildcard *.tex) $(wildcard *.bib)
	- pdflatex -shell-escape $(TARGET)
	- bibtex  $(TARGET)
	- pdflatex -shell-escape $(TARGET)
	pdflatex -shell-escape $(TARGET)

pdf: report.pdf

clean:
	rm -f *.eps
	rm -f *.aux *.log *.out *.bbl *.blg *~ *.bak $(TARGET).ps $(TARGET).pdf
	rm -f *.brf *.lof *.lot *.toc *.glo *.ist
	rm -rf _minted-$(TARGET)

# End
