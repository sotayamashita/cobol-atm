#!/bin/sh
# 回帰テスト。取引結果とエラーコードだけを抜き出して比較可能にする。
set -e
cd "$(dirname "$0")"
rm -f data/atmacct.dat data/atmcard.dat data/atmcash.dat data/atmjrnl.dat
mkdir -p data
./bin/atmseed >/dev/null

filter() { grep -E '取引後残高|現在残高|お引出し可能|手数料|変更しました|円券 x|\[[0-9]{4}\]|お預かり' || true; }

echo "### 1. 照会 → 38,000 円出金 → 10,000 円入金 → 照会"
printf '4900123456780001\n1234\n1\n2\n38000\n3\n10000\n1\n9\n\n' | ./bin/atm | filter

echo "### 2. 1 回限度超過 → 貸越内出金 → 凍結口座へ振込"
printf '4900123456780002\n9999\n2\n60000\n2\n30000\n4\n1000000003\n5000\n9\n\n' | ./bin/atm | filter

echo "### 3. 当日限度額超過 → 千円未満"
printf '4900123456780002\n9999\n2\n50000\n2\n50000\n2\n1500\n9\n\n' | ./bin/atm | filter

echo "### 4. 正常振込 (0001 → 0002)"
printf '4900123456780001\n1234\n4\n1000000002\n20000\n1\n9\n\n' | ./bin/atm | filter

echo "### 5. 暗証番号変更 → 新 PIN で再認証"
printf '4900123456780001\n1234\n5\n1234\n4321\n9\n\n4900123456780001\n4321\n1\n9\n\n' | ./bin/atm | filter

echo "### 6. PIN 3 回誤り → カード閉塞"
printf '4900123456780002\n1111\n2222\n3333\n\n' | ./bin/atm | filter

echo "### 7. 電子ジャーナル (フェーズ/種別/結果/エラー)"
cut -c56-58,159-163 data/atmjrnl.dat
