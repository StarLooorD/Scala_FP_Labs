package lab4


object Huffman {

  /**
   * Код Гаффмана представлений у вигляді бінарного дерева.
   *
   * Кожний `Leaf` дереве представляє один символ абетки які дерево може закодувати.
   * Вага `Leaf` це частота повторень символу.
   *
   * Гілки дерева, вузли `Fork`, представляють множину яка містить всі символи які збережені
   * нижче по дереву на листках. Вага гілки це сума усіх ваг цих листків.
   */
  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree


  // Частина 1: Основи
  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w
    case Leaf(_, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, c, _) => c
    case Leaf(c, _) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree =
    Fork(left, right, chars(left) ++ chars(right), weight(left) + weight(right))

  // Частина 2: Генерування дерев Гаффмана

  /**
   * На даній лабі ми працюємо зі списком символів. Ця функція допоможе перетворити
   * довільну стрічку у список символів.
   */
  def strToChars(str: String): List[Char] = str.toList

  /**
   * Ця функція обчислює кількіть повторень кожного символу зі списку `chars`.
   * Для прикладу, виконання:
   *
   * times(List('a', 'b', 'a'))
   *
   * повинно повернути настпуне (порядок неважливий):
   *
   * List(('a', 2), ('b', 1))
   *
   * Тип `List[(Char, Int)]` означає список пар, де кожна пара складається зі символу і числа.
   * Пари можуть бути легко створені за допомогою дужок:
   *
   * val pair: (Char, Int) = ('c', 1)
   *
   * Щоб отримати будь-яке значення в парі, використовують методи доступу `_1` і `_2`:
   *
   * val theChar = pair._1
   * val theInt  = pair._2
   *
   * Інакший шлях для отримання елементів пари це зіставлення шаблонів:
   *
   * pair match {
   * case (theChar, theInt) =>
   * println("character is: "+ theChar)
   * println("integer is  : "+ theInt)
   * }
   *
   * Або
   *
   * val (theChar, theInt) = pair
   *
   * Увага! Реалізуйте функцію БЕЗ використання groupBy.
   */
  def times(chars: List[Char]): List[(Char, Int)] = {
    var result: List[(Char, Int)] = List()
    chars.foldLeft(Nil: List[Char]) {(acc, next) => if (acc contains next) acc else next :: acc}.foreach(c => {
      var sum: Int = 0
      chars.foreach(ch => if (ch.equals(c)) {sum += 1})
      result = (c, sum) :: result
    })
    result
  }

  /**
   * Повертає список `Leaf` листків для вхідної таблиці `freqs`.
   *
   * Результуючий список повинен бути посортований по зростанню ваг,
   * тобто голова списку повинна містити найменшу вагу. Де вага листка,
   * це частота повторень символу
   *
   * Підказка, для списків є декілька зручних методів які починаються з sort*
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs
    .map { case (c, w) => Leaf(c, w) }
    .sortWith((a, b) => a.weight < b.weight)

  /**
   * Ця функція повинна вставляти дерево `tree` у відсортований по вагам
   * (по зростанню) список `trees` таким чином, щоб порядк зберігся
   */
  def insert(tree: CodeTree, trees: List[CodeTree]): List[CodeTree] = {
    if (trees.length == 0 || weight(tree) <= weight(trees.head))
      tree :: trees
    else
      trees.head :: insert(tree, trees.tail)
  }

  /**
   * Ця функція повинна брати два перші дерева з відсортованого по вагам (по зростанню)
   * списку `trees` і з'єднювати їх в єдиний `Fork` вузол. Використовуючи функцію `insert`,
   * добавити новостворений вузол в список. Виконувати ці операції до тих пір,
   * поки в результаті не залишиться список з одним елементом.
   *
   * Якщо `trees` це список з менше ніж двох елементів, то такий список повинен бути повернутим без змін.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {
    if (trees.length <= 1)
      trees
    else
      combine(insert(new Fork(trees.head, trees.tail.head, chars(trees.head) ::: chars(trees.tail.head), weight(trees.head) + weight(trees.tail.head)),trees.tail.tail))
  }


  /**
   * Ця функція створює кодове дерево, яке є оптимальним для кодування тексту `chars`
   *
   * Текст `chars` є довільним. Ця функція спочатку отримує частоту появлень
   * символів тексту і потім створює кодове дерево за допомогою цього.
   */
  def createCodeTree(chars: List[Char]): CodeTree =
    combine(makeOrderedLeafList(times(chars))).head


  // Частина 3: Декодування

  type Bit = Int

  /**
   * Ця функція декодує список бітів `bits`, використовуючи кодове дерево `tree`
   * і повертає результуючий список декодованих символів.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def traverse(remaining: CodeTree, bits: List[Bit]): List[Char] = remaining match {
      case Leaf(c, _) if bits.isEmpty => List(c)
      case Leaf(c, _) => c :: traverse(tree, bits)
      case Fork(left, right, _, _) if bits.head == 0 => traverse(left, bits.tail)
      case Fork(left, right, _, _) => traverse(right, bits.tail)
    }
    traverse(tree, bits)
  }

  /**
   * Кодове дерево для французької мови, зегеровано за допомогою
   * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * У значенні secret закодована деяка фраза, використайте дерево `frenchCode`
   * для декодування цього коду.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Напишіть функція яка поверає декодований secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Частина 4а: Кодування шляхом проходження по дереву

  /**
   * Ця функція закодовує `text` використовуючи дерево `tree` у послідовність бітів.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def lookup(tree:  CodeTree)(c: Char): List[Bit] = tree match {
      case Leaf(_, _) => List()
      case Fork(left, right, _, _) if chars(left).contains(c) => 0 :: lookup(left)(c)
      case Fork(left, right, _, _) => 1 :: lookup(right)(c)
    }

    text.map(lookup(tree)(_)).flatten
  }


  // Частина 4б: Кодування з використанням кодової таблиці

  type CodeTable = List[(Char, List[Bit])]

  /**
   * Ця функція повертає список бітів який представляє символ `char` в кодовій таблиці `table`
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.filter((code) => code._1 == char ).head._2
  }

  /**
   * На основі кодового дерева створює кодову таблицю,
   * де для кожного символу з дерева, відповідає послідовність бітів.
   *
   * Підказка: подумайте про рекурсивне рішення - кожне піддерево є окремим деревом і
   * для нього можна побудувати таблицю кодування. Викоистовуючи кодові таблиці для піддерев,
   * подумайте як можна побудувати загальну таблицю для всього дерева.
   */
  def convert(tree: CodeTree): CodeTable = {
    val acc: CodeTable = List()
    chars(tree).foldLeft(acc)((acc, char) => (char, encode(tree)(List(char))) :: acc)
  }

  /**
   * Ця функція кодує вхідний `text` використовуючи кодове дерево `tree`.
   *
   * Для пришвидшення процесу кодування, спочатку перетворіть кодове дерево в таблицю,
   * а потім використайте її для кодування самого тексту.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    text.map(codeBits(convert(tree))(_)).flatten
  }
}