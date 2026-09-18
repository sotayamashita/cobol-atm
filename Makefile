COBC      := cobc
COBFLAGS  := -Wall -I copy -std=cobol2002 -ftext-column=250
BIN       := bin
MODULES   := ATMAUTH ATMACCT ATMPOST ATMCASH ATMJRNL ATMCAL ATMZGN

.PHONY: all seed run close test clean journal

all: $(BIN)/atm $(BIN)/atmseed $(BIN)/atmday $(BIN)/caltest $(BIN)/zgntest

# 日次締めバッチ。端末が停止している時間帯に流す
$(BIN)/atmday: src/ATMDAY.cbl src/ATMJRNL.cbl src/ATMCASH.cbl src/ATMRPT.cbl src/ATMCAL.cbl
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

# 時刻依存のモジュールは固定日時を与える単体ドライバで検証する
$(BIN)/caltest: src/CALTEST.cbl src/ATMCAL.cbl
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

$(BIN)/zgntest: src/ZGNTEST.cbl src/ATMZGN.cbl src/ATMCAL.cbl
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

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

# 日次締め。端末が停止している時間帯に流す
close: all
	./$(BIN)/atmday

test: all
	./test.sh

# 電子ジャーナルを読む
journal:
	@cat data/atmjrnl.dat

clean:
	rm -rf $(BIN)
