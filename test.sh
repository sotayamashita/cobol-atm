#!/bin/sh
# 回帰テスト。取引結果とエラーコードだけを抜き出して比較可能にする。
set -e
cd "$(dirname "$0")"
rm -f data/atmacct.dat data/atmcard.dat data/atmcash.dat data/atmjrnl.dat \
  data/atmbank.dat data/atmfee.dat data/atmlimit.dat data/atmhol.dat \
  data/atmclose.dat data/atmrpt.txt
mkdir -p data
./bin/atmseed >/dev/null

filter() { grep -E '取引後残高|現在残高|お引出し可能|手数料|変更しました|円券 x|\[[0-9]{4}\]|お預かり' || true; }

echo "### 1. 照会 → 38,000 円出金 → 10,000 円入金 → 照会"
printf '4900123456780001\n1234\n1\n2\n38000\n3\n10000\n1\n9\n\n' | ./bin/atm | filter

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
# EJ と取引 ID が実行ごとに変わるので、件数と判定結果だけを見る。
close_filter() { grep -E '不確定取引|現金差異|締め処理|\[[0-9]{4}\]|係員' || true; }

echo "-- 正常な締め"
./bin/atmday | close_filter

echo "-- 同じ営業日に再実行 (二重実行の防止)"
./bin/atmday | close_filter

echo "-- 取引の終了レコードを落として再締め (不確定取引の検出)"
python3 - <<'PY'
p = "data/atmjrnl.dat"
lines = open(p, encoding="utf-8").read().splitlines()
out = [x for x in lines if not (len(x) > 57 and x[55] == "E" and x[56:58] == "WD")]
open(p, "w", encoding="utf-8").write("\n".join(out) + "\n")
c = "data/atmclose.dat"
d = open(c, "rb").read()
open(c, "wb").write(d.replace(b"20260918", b"20260917", 1))
PY
./bin/atmday | close_filter
grep -oE '\[(PN|ZU|CD|RF)\] [^ ]+' data/atmrpt.txt || true

echo "### 10. 手数料マスタ (曜日区分 × 時間帯 × カード区分)"
sort data/atmfee.dat

echo "### 11. 限度額マスタ (媒体 × 認証方式)"
cut -c1-27 data/atmlimit.dat
