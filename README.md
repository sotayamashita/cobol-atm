# COBOL ATM

銀行 ATM の端末側アプリケーション。画面はコンソール 1 系統だけだが、
認証・限度額・金種計算・排他制御・電子ジャーナル・取引取消といった
業務ロジックは実機と同じ規律で実装している。

詳細は [docs/design.md](docs/design.md) を参照。

## 構成

| ファイル | 役割 |
| --- | --- |
| `src/ATMMAIN.cbl` | 端末制御。セッション管理と取引の順序制御 |
| `src/ATMAUTH.cbl` | カード認証・PIN・カード閉塞・限度額・当日累計 |
| `src/ATMPOST.cbl` | 取引記帳。検証順序・手数料・残高更新・補償処理 |
| `src/ATMACCT.cbl` | 口座マスタアクセス。排他制御と楽観ロック |
| `src/ATMCASH.cbl` | 紙幣払出。金種計算 (有界ナップサック DP) と在庫 |
| `src/ATMJRNL.cbl` | 電子ジャーナル出力 (追記専用) |
| `src/ATMCAL.cbl` | 営業日カレンダー。曜日区分・祝日・翌営業日 |
| `src/ATMZGN.cbl` | 全銀システム接続。コアタイム / モアタイムの経路判定 |
| `src/ATMCLS.cbl` | 端末状態。締め状態と連番の採番 |
| `src/ATMDAY.cbl` | 日次締めバッチ。不確定取引の抽出と現金突合 |
| `src/ATMRPT.cbl` | 締めレポートの整形出力 |
| `src/ATMSEED.cbl` | 試験用マスタの初期作成 |
| `copy/*.cpy` | レコード定義とモジュール間インタフェース |

## 実行

GnuCOBOL (`cobc`) が必要。

```bash
brew install gnu-cobol
```

```bash
make seed   # マスタを初期化
make run    # ATM を起動
make close  # 日次締めバッチを流す (端末停止中に実行する / 実査枚数を入力)
make load   # カセット装填バッチを流す (端末停止中に実行する)
make purge  # 退避済み EJ を保存年限で整理する (保存年限は実行時に指定)
make test   # 回帰テスト
```

テスト用カード: `4900123456780001` / 暗証番号 `1234`

電子ジャーナルは締めのたびに `data/atmjrnl-YYYYMMDD.dat` へ退避され、
現用ファイルは空になる。退避済みファイルは締めでは消えない。保存年限は
監査要件なので、`make purge` で日数を指定して整理する (既定値は無い)。

電子ジャーナルの確認:

```bash
make journal
```

## トラブルシューティング

リンク時に `ld: tapi error: malformed file ... MacOSX27.0.sdk` が出る場合、
Command Line Tools の `ld` が既定 SDK を解釈できていない。
1 世代前の SDK を指定すればビルドできる。

```bash
SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX26.5.sdk make all
```

## 注意

PIN ハッシュは学習用の簡易な実装で、暗号学的強度を持たない。
実運用には使用しないこと。
