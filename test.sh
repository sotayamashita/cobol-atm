#!/bin/sh
# 回帰テスト。取引結果とエラーコードだけを抜き出して比較可能にする。
set -e
cd "$(dirname "$0")"
rm -f data/atmacct.dat data/atmcard.dat data/atmcash.dat data/atmjrnl.dat \
  data/atmbank.dat data/atmfee.dat data/atmlimit.dat data/atmhol.dat \
  data/atmclose.dat data/atmrpt.txt data/atmjrnl-*.dat
mkdir -p data
./bin/atmseed >/dev/null

filter() { grep -E '取引後残高|現在残高|お引出し可能|手数料|変更しました|円券 x|\[[0-9]{4}\]|お預かり' || true; }

echo "### 1. 照会 → 38,000 円出金 → 10,000 円入金 → 照会"
# 入金は金額ではなく金種ごとの枚数を入れる (1万 x1、5千・2千・千は 0)。
printf '4900123456780001\n1234\n1\n2\n38000\n3\n1\n0\n0\n0\n1\n9\n\n' | ./bin/atm | filter

echo "### 2. 1 回限度超過 → 貸越内出金 → 凍結口座へ振込"
printf '4900123456780002\n9999\n2\n60000\n2\n30000\n4\n1000000003\n5000\n9\n\n' | ./bin/atm | filter

echo "### 3. 当日限度額超過 → 千円未満"
printf '4900123456780002\n9999\n2\n50000\n2\n50000\n2\n1500\n9\n\n' | ./bin/atm | filter

echo "### 4. 自行あて振込 (0001 → 0002)"
printf '4900123456780001\n1234\n4\n\n1000000002\n20000\n1\n9\n\n' | ./bin/atm | filter

echo "### 4b. 他行あて振込 (存在しない行 → 接続不能行)"
printf '4900123456780001\n1234\n4\n9999\n1111111111\n10000\n4\n0005\n1111111111\n10000\n1\n9\n\n' |
  ./bin/atm | filter

echo "### 5. 暗証番号変更 → 新 PIN で再認証"
printf '4900123456780001\n1234\n5\n1234\n4321\n9\n\n4900123456780001\n4321\n1\n9\n\n' | ./bin/atm | filter

echo "### 6. PIN 3 回誤り → カード閉塞"
printf '4900123456780002\n1111\n2222\n3333\n\n' | ./bin/atm | filter

echo "### 7. 電子ジャーナル (フェーズ/種別/結果/エラー)"
cut -c56-58,159-163 data/atmjrnl.dat

# --- ここから第 2 波で追加した機能の検証 -------------------------------
# 曜日区分・手数料・全銀経路は日付と時刻に依存するため、本体を通さず
# 単体ドライバで固定日時を与えて確認する。

echo "### 8. 営業日カレンダー (曜日区分・祝日・翌営業日)"
./bin/caltest

echo "### 9. 全銀システムの経路判定"
./bin/zgntest

echo "### 9b. 日次締め"
# EJ と取引 ID は実行ごとに変わるので、件数と判定結果だけを見る。
# テスト 5 で暗証番号を 4321 に変えているので、以降はそれを使う。
close_filter() {
  grep -E '不確定取引|現金差異|締め処理|EJ 退避|現金実査|\[[0-9]{4}\]|係員' || true
}

# 締めは実査枚数を対話で受け取る。空入力 4 回で未計数 (未実施) になる。
close_uncounted() { printf '\n\n\n\n' | ./bin/atmday; }

jrnl_rows() { wc -l <data/atmjrnl.dat | tr -d ' '; }
arc_rows() { cat data/atmjrnl-*.dat 2>/dev/null | wc -l | tr -d ' '; }

# 締め状態の営業日を前日へ戻す。係員が締め直す状況の再現。
reopen_close_state() {
  python3 -c "
p='data/atmclose.dat'
d=open(p,'rb').read()
open(p,'wb').write(d.replace(b'20260918', b'20260917', 1))
"
}

echo "-- 正常な締め (EJ の退避を含む)"
echo "   締め前の EJ: $(jrnl_rows) 行"
close_uncounted | close_filter
echo "   締め後の EJ: $(jrnl_rows) 行 (退避して空になる)"
echo "   退避先: $(arc_rows) 行"

echo "-- 退避後も通番が続くか (退避済みの最終通番の次から始まる)"
printf '4900123456780001\n4321\n1\n9\n\n' | ./bin/atm >/dev/null 2>&1
echo "   新規レコードの通番: $(cut -c1-9 data/atmjrnl.dat | head -1)"

echo "-- 同じ営業日に再実行 (二重実行の防止)"
close_uncounted | close_filter

echo "-- 既存の退避先を上書きしないか (当日分を失わない)"
reopen_close_state
close_uncounted >/dev/null 2>&1
echo "   退避先: $(arc_rows) 行 (増えも減りもしない)"

echo "-- 取引の終了レコードを落として再締め (不確定取引の検出)"
printf '4900123456780001\n4321\n2\n5000\n9\n\n' | ./bin/atm >/dev/null 2>&1
python3 -c "
p='data/atmjrnl.dat'
lines=open(p,encoding='utf-8').read().splitlines()
out=[x for x in lines if not (len(x)>57 and x[55]=='E' and x[56:58]=='WD')]
open(p,'w',encoding='utf-8').write(chr(10).join(out)+chr(10))
"
reopen_close_state
close_uncounted | close_filter
grep -oE '\[(PN|ZU|CD|RF)\] [^ ]+' data/atmrpt.txt || true

echo "-- 現金実査を入力すると突合が有効になる"
# 帳簿と違う枚数を入れて差異 (CD) を出す。1 本でも未計数なら未実施。
reopen_close_state
printf '1\n' | ./bin/atmday | close_filter
grep -oE '\[(PN|ZU|CD|RF|NC)\] [^ ]+' data/atmrpt.txt || true
reopen_close_state
printf '1\n2\n3\n4\n' | ./bin/atmday | close_filter
grep -oE '\[(PN|ZU|CD|RF|NC)\] [^ ]+' data/atmrpt.txt || true

echo "### 9c. カセット装填"
# 装填は枚数の置換。加算ではないので、装填後の枚数をそのまま入れる。
load_filter() {
  grep -E '円券 x|在庫増減|変更していません|範囲外|正しくありません|\[[0-9]{4}\]' || true
}

echo "-- 帳簿枚数とカセットの状態を示す (障害中のカセットを含む)"
python3 -c "
p='data/atmcash.dat'
d=bytearray(open(p,'rb').read()); i=d.index(b'ATM00001'); d[i+66:i+67]=b'F'
open(p,'wb').write(d)
"
printf '\n\n\n\n' | ./bin/atmload | load_filter
./bin/atmseed >/dev/null

echo "-- 2 千券を 150 枚、千券を 500 枚に装填"
printf '\n\n150\n500\n' | ./bin/atmload | load_filter

echo "-- 全て空入力 (在庫も EJ も動かさない)"
printf '\n\n\n\n' | ./bin/atmload | load_filter

echo "-- 負数・非数値・範囲外は弾く"
printf -- '-5\nabc\n999999\n\n' | ./bin/atmload | load_filter

echo "-- EJ に装填が残るか (LD)"
grep -c 'LD' data/atmjrnl.dat

echo "### 9d. 障害カセットへの入金は受け付けない"
# カセット 3 (2 千券) の状態を F にする。実際に障害を起こす手段が
# 無いので、在庫ファイルを直接書き換えて再現する。
python3 -c "
p='data/atmcash.dat'
d=bytearray(open(p,'rb').read())
i=d.index(b'ATM00001')
d[i+66:i+67]=b'F'
open(p,'wb').write(d)
"
# テスト 5 で暗証番号を 4321 に変えているので、以降はそれを使う。
printf '4900123456780001\n4321\n3\n0\n0\n1\n0\n9\n\n' | ./bin/atm | filter
./bin/atmseed >/dev/null

echo "### 9e. 退避 EJ の保存年限管理"
# 退避ファイルを人工的に用意する。古い 3 本と、保存年限内の 1 本。
for d in 20200101 20200102 20240301 20260917; do echo dummy >"data/atmjrnl-$d.dat"; done
purge_filter() { grep -E '対象|削除|中止|日数|保存年限が' || true; }

echo "-- 保存年限を指定しなければ何もしない"
printf '\n' | ./bin/atmpurge | purge_filter

echo "-- 確認で Y 以外なら消さない"
printf '365\nN\n' | ./bin/atmpurge | purge_filter
find data -name 'atmjrnl-2*.dat' | wc -l | tr -d ' '

echo "-- 確認で Y なら消す (保存年限内の 1 本は残る)"
printf '365\nY\n' | ./bin/atmpurge | purge_filter
find data -name 'atmjrnl-2*.dat' | sort

echo "-- 範囲外・非数値は弾く"
printf -- '-1\n' | ./bin/atmpurge | purge_filter
printf 'abc\n' | ./bin/atmpurge | purge_filter
rm -f data/atmjrnl-2*.dat

echo "### 10. 手数料マスタ (曜日区分 × 時間帯 × カード区分)"
sort data/atmfee.dat

echo "### 11. 限度額マスタ (媒体 × 認証方式)"
cut -c1-27 data/atmlimit.dat

echo "### 12b. 勘定系ホストが応答しない場合"
# 口座 1000000009 への記帳に ATMHOST は黙る。端末は取引照会で決着させ、
# 記帳されていないと判れば取引を失敗として閉じる (現金は出さない)。
printf '4900123456780009\n1234\n2\n10000\n1\n9\n\n' | ./bin/atm | filter
echo "   残高が動いていないこと:"
printf '4900123456780009\n1234\n1\n9\n\n' | ./bin/atm | grep '現在残高'

echo "### 12. 営業時間マスタ (曜日区分 × 時間帯)"
# 既定は 24 時間稼働。行を消すと規制なし、終日休止は 0000-0000 で表す。
cut -c1-9 data/atmhour.dat
