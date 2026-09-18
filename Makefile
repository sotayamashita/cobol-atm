COBC      := cobc
COBFLAGS  := -Wall -I copy -std=cobol2002 -ftext-column=250
BIN       := bin
MODULES   := ATMAUTH ATMACCT ATMPOST ATMCASH ATMJRNL ATMCAL

.PHONY: all seed run clean journal

all: $(BIN)/atm $(BIN)/atmseed

$(BIN)/atm: src/ATMMAIN.cbl $(addprefix src/,$(addsuffix .cbl,$(MODULES)))
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

$(BIN)/atmseed: src/ATMSEED.cbl src/ATMAUTH.cbl
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

# マスタを初期状態に戻す (既存の残高・ジャーナルは消える)
seed: $(BIN)/atmseed
	@rm -f data/atmacct.dat data/atmcard.dat data/atmcash.dat
	./$(BIN)/atmseed

run: all
	./$(BIN)/atm

# 電子ジャーナルを読む
journal:
	@cat data/atmjrnl.dat

clean:
	rm -rf $(BIN)
