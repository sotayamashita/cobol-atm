# COBOL ATM

銀行 ATM の端末側アプリケーション。画面はコンソール 1 系統だけだが、
認証・限度額・金種計算・排他制御・電子ジャーナル・取引取消といった
業務ロジックは実機と同じ規律で実装している。

口座元帳は勘定系ホストが持ち、端末は電文でのみ触れる。ホストと全銀
システムはいずれも同一プロセス内のモジュールとして模擬する。

詳細は [docs/design.md](docs/design.md) を参照。

## 構成

### 端末

| ファイル | 役割 |
| --- | --- |
| `src/ATMMAIN.cbl` | 端末制御。セッション管理と取引の順序制御 |
| `src/ATMAUTH.cbl` | カード認証・PIN・カード閉塞・限度額・当日累計 |
| `src/ATMPOST.cbl` | 取引記帳。検証順序・手数料・残高更新・補償処理 |
| `src/ATMACCT.cbl` | 口座アクセス。ホスト接続と成否不明の解決 |
| `src/ATMCASH.cbl` | 現金機構。金種計算 (有界ナップサック DP)・払出・収納・在庫 |
| `src/ATMJRNL.cbl` | 電子ジャーナル。追記・走査・退避・保存年限 |
| `src/ATMCAL.cbl` | 営業日カレンダー。曜日区分・祝日・翌営業日 |
| `src/ATMCLS.cbl` | 端末状態。締め状態と連番の採番 |
| `src/ATMENV.cbl` | 端末 ID の解決 (`ATM_ID` / 既定は `ATM00001`) |

### 対外系 (模擬)

| ファイル | 役割 |
| --- | --- |
| `src/ATMHOST.cbl` | 勘定系ホスト。口座元帳の所有者。冪等な記帳と成否不明 |
| `src/ATMZGN.cbl` | 全銀システム。コアタイム / モアタイムの経路判定 |

### 運用バッチ (端末停止中に流す)

| ファイル | 役割 |
| --- | --- |
| `src/ATMDAY.cbl` | 日次締め。不確定取引の抽出・現金実査の突合・繰越 |
| `src/ATMRPT.cbl` | 締めレポートの整形出力 |
| `src/ATMLOAD.cbl` | カセット装填 |
| `src/ATMPURGE.cbl` | 退避済み電子ジャーナルの保存年限管理 |
| `src/ATMSEED.cbl` | 試験用マスタの初期作成 |

`copy/*.cpy` はレコード定義とモジュール間インタフェース。

## 実行

GnuCOBOL (`cobc`) が必要。

```bash
brew install gnu-cobol
```

```bash
make seed   # マスタを初期化
make run    # ATM を起動
make close  # 日次締め (実査枚数を入力する)
make load   # カセット装填 (装填後の枚数を入力する)
make purge  # 退避済み EJ の保存年限管理 (日数を指定する / 既定値は無い)
make test   # 回帰テスト
```

テスト用カード: `4900123456780001` / 暗証番号 `1234`

運用バッチ (`close` / `load` / `purge`) は端末が停止している時間帯に流す。
オンライン中に走らせると在庫と電子ジャーナルが動き続け、突合した瞬間の値が
意味を持たない。

## 複数端末

端末 ID は環境変数 `ATM_ID` で与える (既定は `ATM00001`)。

```bash
ATM_ID=ATM00002 make seed   # 2 台目のカセットと締め状態を作る
ATM_ID=ATM00002 make run
```

現金カセット・電子ジャーナル・締め状態・帳票は端末ごとに分かれ
(`data/atmcash-ATM00001.dat` のように端末 ID が入る)、口座元帳とカード・
各種マスタは全端末で共有する。

> `make seed` は共有マスタ (口座・カード) も書き直すので、2 台目を用意
> すると口座残高が初期値へ戻る。端末ごとのカセットは `ATM_ID` が指すもの
> だけを作り直すため、1 台目の在庫は残る。

## 電子ジャーナル

締めのたびに `data/atmjrnl-<端末ID>-YYYYMMDD.dat` へ退避され、現用ファイルは
空になる。退避済みファイルは締めでは消えない。保存年限は監査要件なので、
`make purge` で日数を指定して整理する。

```bash
make journal                    # 現用ファイルを読む
ATM_ID=ATM00002 make journal    # 2 台目を読む
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
