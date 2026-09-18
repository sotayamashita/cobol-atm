COBC      := cobc
COBFLAGS  := -Wall -I copy -std=cobol2002 -ftext-column=250
BIN       := bin
MODULES   := ATMAUTH ATMACCT ATMHOST ATMPOST ATMCASH ATMJRNL ATMCAL \
             ATMZGN ATMCLS ATMENV

.PHONY: all seed run close load purge test clean journal

all: $(BIN)/atm $(BIN)/atmseed $(BIN)/atmday $(BIN)/atmload \
     $(BIN)/atmpurge $(BIN)/caltest $(BIN)/zgntest

# 日次締めバッチ。端末が停止している時間帯に流す
$(BIN)/atmday: src/ATMDAY.cbl src/ATMJRNL.cbl src/ATMCASH.cbl src/ATMRPT.cbl \
               src/ATMCLS.cbl src/ATMENV.cbl
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

# カセット装填バッチ。締めと同じく端末が停止している時間帯に流す
$(BIN)/atmload: src/ATMLOAD.cbl src/ATMCASH.cbl src/ATMJRNL.cbl src/ATMCLS.cbl \
                src/ATMENV.cbl
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

# 退避済み EJ の保存年限管理。保存年限は実行時に指定する (既定値なし)
$(BIN)/atmpurge: src/ATMPURGE.cbl src/ATMJRNL.cbl src/ATMCLS.cbl src/ATMENV.cbl
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

$(BIN)/atmseed: src/ATMSEED.cbl src/ATMAUTH.cbl src/ATMENV.cbl
	@mkdir -p $(BIN) data
	$(COBC) -x $(COBFLAGS) -o $@ $^

# マスタを初期状態に戻す (既存の残高は消える)。
# 消すのは共有マスタと、ATM_ID が指す端末のカセットだけ。全端末分を
# 消すと、2 台目を用意しただけで 1 台目の在庫が飛ぶ。
seed: $(BIN)/atmseed
	@rm -f data/atmacct.dat data/atmcard.dat \
	       data/atmcash-$${ATM_ID:-ATM00001}.dat
	./$(BIN)/atmseed

run: all
	./$(BIN)/atm

# 日次締め。端末が停止している時間帯に流す
close: all
	./$(BIN)/atmday

# カセット装填。端末が停止している時間帯に流す
load: all
	./$(BIN)/atmload

# 退避済み EJ の保存年限管理。保存年限を対話で指定する
purge: all
	./$(BIN)/atmpurge

test: all
	./test.sh

# 電子ジャーナルを読む。端末ごとに分かれるので ATM_ID に従う。
journal:
	@cat data/atmjrnl-$${ATM_ID:-ATM00001}.dat

clean:
	rm -rf $(BIN)
