HEAPIMAGE = relplot.x86-darwin
default: $(HEAPIMAGE) timeout
SML=$(SMLHOME)/bin/sml
#SMLHOME=/usr/local/smlnj
SMLHOME=$(HOME)


DIRT = eqn.grm.* eqn.lex.*

clean:
	rm -f $(DIRT)
	rm -r .cm CM

$(HEAPIMAGE): relplot.sml eqn.grm eqn.sml interval.sml parser.sml eqn.lex erf.sml
	echo 'CM.make("sources.cm");' | $(SML)

timeout: timeout.c
	$(CC) -o timeout timeout.c
