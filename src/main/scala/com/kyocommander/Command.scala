package com.kyocommander

sealed trait CommandType
case class NL(nums: Seq[Int]) extends CommandType
case class SL(strs: Seq[String]) extends CommandType
case class SNL(strs: Seq[String], nums: Seq[Int]) extends CommandType
case class Nop() extends CommandType


case class Command(str: String, commandType: CommandType)

object Commands {

  def commandString(sn: String) = "@" + sn + seqr(san) + "、" + seqr(prefixes) + valueString + seqr(saffix)  
  
  private def valueString = seqr(values) match {
    case Command(s, NL(seq)) => s.replaceAll("\\$0", seqr(seq).toString)
    case Command(s, SL(seq)) => s.replaceAll("\\$s", seqr(seq))
    case Command(s, SNL(strs, nums)) => s.replaceAll("\\$s", seqr(strs)).replaceAll("\\$0", seqr(nums).toString)
    case Command(s, Nop()) => s
  }
  
  private val r = new java.util.Random
  private def seqr[A](seq: Seq[A]):A = seq(r.nextInt(seq.size))
  private def ns(n: Int) = Range(1,n+1).toSeq
  private def ns10(n: Int) = Range(10, n+1, 10)
  private def ss(s: String) = s.split(",").toSeq
  
  private val san = Seq[String]("さん")
  
  private val prefixes = Seq[String](
      "あなたは今日、",
      "あなたは本日、",
      "今日の行動を指示するわ。あなたは、",
      "あなたに今日の行動司令が下ったわ。")
  private val saffix = Seq[String](
      "。いいわね？すぐに取り掛かりなさい",
      "。いいわね？必ずやるのよ",
      "。いいわね？これは決定事項よ",
      "。いいわね？絶対よ",
      "。すぐにやるのよ",
      "。必ずやるのよ",
      "。これは決定事項よ",
      "。絶対よ",
      "。わかった？すぐに取り掛かりなさい",
      "。わかった？必ずやるのよ",
      "。わかった？これは決定事項よ",
      "。わかった？絶対よ"
      )

  private val values = Seq[Command](
    // Nop
    Command("布団を干しなさい", Nop()),
    Command("映画を観に行きなさい", Nop()),
    Command("映画館でポップコーンを買いなさい", Nop()),
    Command("自動販売機で一番不味そうなジュースを買って飲みなさい", Nop()),
    Command("何てもいいから新しいWebサービスの会員登録しなさい", Nop()),
    Command("好きな人に気持ちを伝えなさい", Nop()),
    Command("通過するポストの数を数えなさい", Nop()),
    Command("通過する電柱の数を数えなさい", Nop()),
    Command("早く布団に入ってゆっくり休みなさい", Nop()),
    Command("有給休暇の申請をしなさい", Nop()),
    Command("漢文を読みなさい", Nop()),
    Command("動画を撮影してYoutubeにアップしなさい", Nop()),
    Command("買ってプレイしていないゲームをやりなさい", Nop()),
    Command("買って読んでいない本を読みなさい", Nop()),
    Command("地下鉄に乗って暗闇を見つめなさい", Nop()),
    Command("鳥を探して数を数えなさい", Nop()),
    Command("あなたの夢を紙に書きなさい", Nop()),
    Command("あなたの夢を友達に話しなさい", Nop()),
    Command("賃貸サイトで理想の物件を探しなさい", Nop()),
    Command("転職サイトで理想の転職先を探しなさい", Nop()),
    Command("通販サイトで欲しい物をお気に入りリストに入れなさい", Nop()),
    Command("一発ギャグを考えて誰かに披露しなさい", Nop()),
    Command("小説のシナリオを考えて誰かに話しなさい", Nop()),
    Command("宝くじで一等が当たったら何をするか考えなさい", Nop()),
    Command("口紅を塗って外出しなさい", Nop()),
    Command("アイラインを塗って外出しなさい", Nop()),
    Command("お化粧をせずに外出しなさい", Nop()),
    Command("浜辺で「バカヤロー」と叫びなさい", Nop()),
    Command("夕日を沈むまで眺めなさい", Nop()),
    Command("小学校の友達に連絡しなさい", Nop()),
    Command("飲み会を企画しなさい", Nop()),
    Command("コスプレしなさい", Nop()),
    Command("女装(男装)しなさい", Nop()),
    Command("社長の椅子に座りなさい", Nop()),
    Command("お米を1合取って何粒あるか数えなさい", Nop()),
    Command("畳の目の数を数えなさい", Nop()),
    Command("昆虫を捕まえなさい", Nop()),
    Command("子供の名前を考えなさい", Nop()),
    Command("名言を調べて覚えなさい", Nop()),
    Command("知らない偉人の名前を10人覚えなさい", Nop()),
    Command("屋台で食事しなさい", Nop()),
    Command("絵を描いてアップしなさい", Nop()),
    Command("何もしないでいなさい", Nop()),
    Command("ゆっくり休みなさい", Nop()),
    Command("自宅を警備しなさい", Nop()),
    Command("ぼーっとしなさい", Nop()),
    Command("行ったことの無い町に行きなさい", Nop()),
    Command("入ったことの無い店に行って食事しなさい", Nop()),
    Command("お花を買って誰かに渡しなさい", Nop()),
    Command("部屋から一歩も出ずに一日を過ごしなさい", Nop()),
    Command("魚を食べなさい", Nop()),
    Command("自分のオリジナルダンスを披露しなさい", Nop()),
    Command("新聞を読みなさい", Nop()),
    Command("散歩しなさい", Nop()),
    Command("カフェで読書しなさい", Nop()),
    // NL
    Command("お風呂に$0回はいりなさい", NL(ns(3))),
    Command("数式を$0問解きなさい", NL(ns10(100))),
    Command("英単語を$0個覚えなさい", NL(ns10(100))),
    Command("電車に乗って$0駅目で降りてぶらり散歩旅しなさい", NL(ns(10))),
    Command("$0分以上ライブ配信しなさい", NL(ns(5))),
    Command("ビデオゲームで$0時間以上遊びなさい", NL(ns(5))),
    Command("$0回深呼吸しなさい", NL(ns10(20))),
    Command("野菜サラダを$0人前以上食べなさい", NL(ns(3))),
    // SL
    Command("好きな$sを読みなさい", SL(ss("本,漫画,小説,ラノベ,偉人の伝記,ノンフィクション,作家の本,作家の漫画,作家の小説"))),
    Command("好きな$sを買いなさい", SL(ss("本,漫画,小説,ラノベ,偉人の伝記,ノンフィクション,作家の本,作家の漫画,作家の小説"))),
    Command("$sの掃除をすること", SL(ss("部屋,トイレ,玄関,机,本棚,お風呂,台所,ソファー,テレビ周り,パソコン周り,フォルダ,デスクトップ,不要なアプリ"))),
    Command("$sのお祓いをすること", SL(ss("部屋,トイレ,玄関,机,本棚,お風呂,台所,ソファー,テレビ周り,パソコン周り"))),
    Command("テレビで$sを観なさい", SL(ss("ドラマ,お笑い番組,ニュース,ドキュメンタリー,教育番組,アニメ,スポーツ,好きな番組,映画,CMを10本"))),
    Command("テレビで$sな芸能人を探しなさい", SL(ss("好き,嫌い,落ち目,売れっ子,可哀想,おバカ,お調子者"))),
    Command("$sを食べなさい", SL(ss("カレー,ラーメン,うどん,カツ丼,ミネストローネ,ケバブ,虫料理,激辛料理,中華料理,イタリアン,スパゲッティ,ナポリタン,ミートソース,ソース焼きそば,おにぎり,おにぎらず,天ぷら,天丼,洋食,海軍カレー,ピーマン,ニンジン,しいたけ,ナス,トマト,チーズ,蓮根"))),
    Command("$sを作りなさい", SL(ss("料理,餃子,ハンバーグ,創作料理,お気に入りの場所"))),
    Command("$sに電話しなさい", SL(ss("母親,父親,親,好きな人,嫌いな人,相棒,友達,親友"))),
    Command("$sにメッセージを送りなさい", SL(ss("母親,父親,親,好きな人,嫌いな人,相棒,友達,親友"))),
    Command("$sに行きなさい", SL(ss("カラオケ,学校,会社,コンビニ,銭湯,温泉,浜辺,河原,思い出の場所,見晴らしの良い所,公衆トイレ,お寺,神社,教会,病院,ドラッグストア,スーパー,コインランドリー,世界の中心,パン屋,お気に入りの場所,クラブ,ゴルフの打ちっぱなし,メイド喫茶,競馬場,競輪場,競艇場,オートレース場,ディズニーランド,公園,図書館"))),
    Command("$sを書きなさい", SL(ss("反省文,恋文,短編小説,漫画,イラスト,オリジナルキャラ,日記,お笑いのネタ"))),
    Command("サイコロを2個振り、出た目の数×10だけ$sしなさい", SL(ss("腹筋,スクワット,腕立て伏せ,ジャンプ,しっぺ,絶望,笑顔の練習を,ツイート"))),
    Command("$sの勉強をしなさい", SL(ss("英語,数学,プログラミング,デザイン,世界史,日本史,保健体育,為替,イスラム教,仏教,ユダヤ教,キリスト教"))),
    Command("友達と$sをして遊びなさい", SL(ss("トランプ,ウノ,将棋,囲碁,麻雀,ビデオゲーム,相撲,鬼ごっこ,ゴルフ,しりとり,山手線ゲーム,王様ゲーム,ビリヤード,ダーツ,昆虫採集"))),
    Command("$sの写真を取ってアップしなさい", SL(ss("飛行機,スポーツカー,ロードバイク,お花,変なもの,面白いもの,仏像,お地蔵様,空,道,雑踏,電柱,標識,雑草,丸いもの,四角いもの,白いもの,赤いもの,黒いもの,あなたを省庁するもの"))),
    Command("$sを撫でなさい", SL(ss("猫,犬,モルモット,ハムスター,陶磁器,車,机,誰かの頭"))),
    Command("好きな$sについて熱く語りなさい", SL(ss("小説,漫画,ゲーム,スポーツ,スポーツ選手,格闘技,格闘家,職業"))),
    Command("$sに触りなさい", SL(ss("氷,雪,海水,砂,ポスト,電柱,地面,白線,ガードレール,植木,昆虫,魚,小動物,お花,手すり,一万円札,小石,自転車,車,ベンチ,ミミズ"))),
    Command("$sの味比べをしなさい", SL(ss("缶コーヒー,レトルトカレー,肉まん,プリン"))),
    Command("$sを観に行きなさい", SL(ss("ヒーローショー,映画,スポーツ"))),

    Command("$sを$0人フォローしなさい", SNL(ss("アメリカ人,カナダ人,イタリア人,フランス人,ドイツ人,スペイン人,オランダ人,インド人,中国人,韓国人"), ns(10))),
    Command("「$s」という字を$0回書き取りしなさい", SNL(ss("漢,薔薇,醤油,一"),ns10(100))),
    Command("知らない$sの名前を$0個覚えなさい",SNL(ss("魚,動物,植物,星,駅,英単語,英語の言い回し,ネットスラング"),ns10(50)))
  )
}



