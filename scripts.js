var dictionary = [
     "ability", "able", "aboard", "about", "above", "accept", "accident", "according",
     "account", "accurate", "acres", "across", "act", "action", "active", "activity",
     "actual", "actually", "add", "addition", "additional", "adjective", "adult", "adventure",
     "advice", "affect", "afraid", "after", "afternoon", "again", "against", "age",
     "ago", "agree", "ahead", "aid", "air", "airplane", "alike", "alive",
     "all", "allow", "almost", "alone", "along", "aloud", "alphabet", "already",
     "also", "although", "am", "among", "amount", "ancient", "angle", "angry",
     "animal", "announced", "another", "answer", "ants", "any", "anybody", "anyone",
     "anything", "anyway", "anywhere", "apart", "apartment", "appearance", "apple", "applied",
     "appropriate", "are", "area", "arm", "army", "around", "arrange", "arrangement",
     "arrive", "arrow", "art", "article", "as", "aside", "ask", "asleep",
     "at", "ate", "atmosphere", "atom", "atomic", "attached", "attack", "attempt",
     "attention", "audience", "author", "automobile", "available", "average", "avoid", "aware",
     "away", "baby", "back", "bad", "badly", "bag", "balance", "ball",
     "balloon", "band", "bank", "bar", "bare", "bark", "barn", "base",
     "baseball", "basic", "basis", "basket", "bat", "battle", "be", "bean",
     "bear", "beat", "beautiful", "beauty", "became", "because", "become", "becoming",
     "bee", "been", "before", "began", "beginning", "begun", "behavior", "behind",
     "being", "believed", "bell", "belong", "below", "belt", "bend", "beneath",
     "bent", "beside", "best", "bet", "better", "between", "beyond", "bicycle",
     "bigger", "biggest", "bill", "birds", "birth", "birthday", "bit", "bite",
     "black", "blank", "blanket", "blew", "blind", "block", "blood", "blow",
     "blue", "board", "boat", "body", "bone", "book", "border", "born",
     "both", "bottle", "bottom", "bound", "bow", "bowl", "box", "boy",
     "brain", "branch", "brass", "brave", "bread", "break", "breakfast", "breath",
     "breathe", "breathing", "breeze", "brick", "bridge", "brief", "bright", "bring",
     "broad", "broke", "broken", "brother", "brought", "brown", "brush", "buffalo",
     "build", "building", "built", "buried", "burn", "burst", "bus", "bush",
     "business", "busy", "but", "butter", "buy", "by", "cabin", "cage",
     "cake", "call", "calm", "came", "camera", "camp", "can", "canal",
     "cannot", "cap", "capital", "captain", "captured", "car", "carbon", "card",
     "care", "careful", "carefully", "carried", "carry", "case", "cast", "castle",
     "cat", "catch", "cattle", "caught", "cause", "cave", "cell", "cent",
     "center", "central", "century", "certain", "certainly", "chain", "chair", "chamber",
     "chance", "change", "changing", "chapter", "character", "characteristic", "charge", "chart",
     "check", "cheese", "chemical", "chest", "chicken", "chief", "child", "children",
     "choice", "choose", "chose", "chosen", "church", "circle", "circus", "citizen",
     "city", "class", "classroom", "claws", "clay", "clean", "clear", "clearly",
     "climate", "climb", "clock", "close", "closely", "closer", "cloth", "clothes",
     "clothing", "cloud", "club", "coach", "coal", "coast", "coat", "coffee",
     "cold", "collect", "college", "colony", "color", "column", "combination", "combine",
     "come", "comfortable", "coming", "command", "common", "community", "company", "compare",
     "compass", "complete", "completely", "complex", "composed", "composition", "compound", "concerned",
     "condition", "congress", "connected", "consider", "consist", "consonant", "constantly", "construction",
     "contain", "continent", "continued", "contrast", "control", "conversation", "cook", "cookies",
     "cool", "copper", "copy", "corn", "corner", "correct", "correctly", "cost",
     "cotton", "could", "count", "country", "couple", "courage", "course", "court",
     "cover", "cow", "cowboy", "crack", "cream", "create", "creature", "crew",
     "crop", "cross", "crowd", "cry", "cup", "curious", "current", "curve",
     "customs", "cut", "cutting", "daily", "damage", "dance", "danger", "dangerous",
     "dark", "darkness", "date", "daughter", "dawn", "day", "dead", "deal",
     "dear", "death", "decide", "declared", "deep", "deeply", "deer", "definition",
     "degree", "depend", "depth", "describe", "desert", "design", "desk", "detail",
     "determine", "develop", "development", "diagram", "diameter", "did", "die", "differ",
     "difference", "different", "difficult", "difficulty", "dig", "dinner", "direct", "direction",
     "directly", "dirt", "dirty", "disappear", "discover", "discovery", "discuss", "discussion",
     "disease", "dish", "distance", "distant", "divide", "division", "do", "doctor",
     "does", "dog", "doing", "doll", "dollar", "done", "donkey", "door",
     "dot", "double", "doubt", "down", "dozen", "draw", "drawn", "dream",
     "dress", "drew", "dried", "drink", "drive", "driven", "driver", "driving",
     "drop", "dropped", "drove", "dry", "duck", "due", "dug", "dull",
     "during", "dust", "duty", "each", "eager", "ear", "earlier", "early",
     "earn", "earth", "easier", "easily", "east", "easy", "eat", "eaten",
     "edge", "education", "effect", "effort", "egg", "eight", "either", "electric",
     "electricity", "element", "elephant", "eleven", "else", "empty", "end", "enemy",
     "energy", "engine", "engineer", "enjoy", "enough", "enter", "entire", "entirely",
     "environment", "equal", "equally", "equator", "equipment", "escape", "especially", "essential",
     "establish", "even", "evening", "event", "eventually", "ever", "every", "everybody",
     "everyone", "everything", "everywhere", "evidence", "exact", "exactly", "examine", "example",
     "excellent", "except", "exchange", "excited", "excitement", "exciting", "exclaimed", "exercise",
     "exist", "expect", "experience", "experiment", "explain", "explanation", "explore", "express",
     "expression", "extra", "eye", "face", "facing", "fact", "factor", "factory",
     "failed", "fair", "fairly", "fall", "fallen", "familiar", "family", "famous",
     "far", "farm", "farmer", "farther", "fast", "fastened", "faster", "fat",
     "father", "favorite", "fear", "feathers", "feature", "fed", "feed", "feel",
     "feet", "fell", "fellow", "felt", "fence", "few", "fewer", "field",
     "fierce", "fifteen", "fifth", "fifty", "fight", "fighting", "figure", "fill",
     "film", "final", "finally", "find", "fine", "finest", "finger", "finish",
     "fire", "fireplace", "firm", "first", "fish", "five", "fix", "flag",
     "flame", "flat", "flew", "flies", "flight", "floating", "floor", "flow",
     "flower", "fly", "fog", "folks", "follow", "food", "foot", "football",
     "for", "force", "foreign", "forest", "forget", "forgot", "forgotten", "form",
     "former", "fort", "forth", "forty", "forward", "fought", "found", "four",
     "fourth", "fox", "frame", "free", "freedom", "frequently", "fresh", "friend",
     "friendly", "frighten", "frog", "from", "front", "frozen", "fruit", "fuel",
     "full", "fully", "fun", "function", "funny", "fur", "furniture", "further",
     "future", "gain", "game", "garage", "garden", "gas", "gasoline", "gate",
     "gather", "gave", "general", "generally", "gentle", "gently", "get", "getting",
     "giant", "gift", "girl", "give", "given", "giving", "glad", "glass",
     "globe", "go", "goes", "gold", "golden", "gone", "good", "goose",
     "got", "government", "grabbed", "grade", "gradually", "grain", "grandfather", "grandmother",
     "graph", "grass", "gravity", "gray", "great", "greater", "greatest", "greatly",
     "green", "grew", "ground", "group", "grow", "grown", "growth", "guard",
     "guess", "guide", "gulf", "gun", "habit", "had", "hair", "half",
     "halfway", "hall", "hand", "handle", "handsome", "hang", "happen", "happened",
     "happily", "happy", "harbor", "hard", "harder", "hardly", "has", "hat",
     "have", "having", "hay", "he", "headed", "heading", "health", "heard",
     "hearing", "heart", "heat", "heavy", "height", "held", "hello", "help",
     "helpful", "her", "herd", "here", "herself", "hidden", "hide", "high",
     "higher", "highest", "highway", "hill", "him", "himself", "his", "history",
     "hit", "hold", "hole", "hollow", "home", "honor", "hope", "horn",
     "horse", "hospital", "hot", "hour", "house", "how", "however", "huge",
     "human", "hundred", "hung", "hungry", "hunt", "hunter", "hurried", "hurry",
     "hurt", "husband", "ice", "idea", "identity", "if", "ill", "image",
     "imagine", "immediately", "importance", "important", "impossible", "improve", "in", "inch",
     "include", "including", "income", "increase", "indeed", "independent", "indicate", "individual",
     "industrial", "industry", "influence", "information", "inside", "instance", "instant", "instead",
     "instrument", "interest", "interior", "into", "introduced", "invented", "involved", "iron",
     "is", "island", "it", "its", "itself", "jack", "jar", "jet",
     "job", "join", "joined", "journey", "joy", "judge", "jump", "jungle",
     "just", "keep", "kept", "key", "kids", "kill", "kind", "kitchen",
     "knew", "knife", "know", "knowledge", "known", "label", "labor", "lack",
     "lady", "laid", "lake", "lamp", "land", "language", "large", "larger",
     "largest", "last", "late", "later", "laugh", "law", "lay", "layers",
     "lead", "leader", "leaf", "learn", "least", "leather", "leave", "leaving",
     "led", "left", "leg", "length", "lesson", "let", "letter", "level",
     "library", "lie", "life", "lift", "light", "like", "likely", "limited",
     "line", "lion", "lips", "liquid", "list", "listen", "little", "live",
     "living", "load", "local", "locate", "location", "log", "lonely", "long",
     "longer", "look", "loose", "lose", "loss", "lost", "lot", "loud",
     "love", "lovely", "low", "lower", "luck", "lucky", "lunch", "lungs",
     "lying", "machine", "machinery", "mad", "made", "magic", "magnet", "mail",
     "main", "mainly", "major", "make", "making", "man", "managed", "manner",
     "manufacturing", "many", "map", "mark", "market", "married", "mass", "massage",
     "master", "material", "mathematics", "matter", "may", "maybe", "me", "meal",
     "mean", "means", "meant", "measure", "meat", "medicine", "meet", "melted",
     "member", "memory", "men", "mental", "merely", "met", "metal", "method",
     "mice", "middle", "might", "mighty", "mile", "military", "milk", "mill",
     "mind", "mine", "minerals", "minute", "mirror", "missing", "mission", "mistake",
     "mix", "mixture", "model", "modern", "molecular", "moment", "money", "monkey",
     "month", "mood", "moon", "more", "morning", "most", "mostly", "mother",
     "motion", "motor", "mountain", "mouse", "mouth", "move", "movement", "movie",
     "moving", "mud", "muscle", "music", "musical", "must", "my", "myself",
     "mysterious", "nails", "name", "nation", "national", "native", "natural", "naturally",
     "nature", "near", "nearby", "nearer", "nearest", "nearly", "necessary", "neck",
     "needed", "needle", "needs", "negative", "neighbor", "neighborhood", "nervous", "nest",
     "never", "new", "news", "newspaper", "next", "nice", "night", "nine",
     "no", "nobody", "nodded", "noise", "none", "noon", "nor", "north",
     "nose", "not", "note", "noted", "nothing", "notice", "noun", "now",
     "number", "numeral", "nuts", "object", "observe", "obtain", "occasionally", "occur",
     "ocean", "of", "off", "offer", "office", "officer", "official", "oil",
     "old", "older", "oldest", "on", "once", "one", "only", "onto",
     "open", "operation", "opinion", "opportunity", "opposite", "or", "orange", "orbit",
     "order", "ordinary", "organization", "organized", "origin", "original", "other", "ought",
     "our", "ourselves", "out", "outer", "outline", "outside", "over", "own",
     "owner", "oxygen", "pack", "package", "page", "paid", "pain", "paint",
     "pair", "palace", "pale", "pan", "paper", "paragraph", "parallel", "parent",
     "park", "part", "particles", "particular", "particularly", "partly", "parts", "party",
     "pass", "passage", "past", "path", "pattern", "pay", "peace", "pen",
     "pencil", "people", "per", "percent", "perfect", "perfectly", "perhaps", "period",
     "person", "personal", "pet", "phrase", "physical", "piano", "pick", "picture",
     "pictured", "pie", "piece", "pig", "pile", "pilot", "pine", "pink",
     "pipe", "pitch", "place", "plain", "plan", "plane", "planet", "planned",
     "planning", "plant", "plastic", "plate", "plates", "play", "pleasant", "please",
     "pleasure", "plenty", "plural", "plus", "pocket", "poem", "poet", "poetry",
     "point", "pole", "police", "policeman", "political", "pond", "pony", "pool",
     "poor", "popular", "population", "porch", "port", "position", "positive", "possible",
     "possibly", "post", "pot", "potatoes", "pound", "pour", "powder", "power",
     "powerful", "practical", "practice", "prepare", "present", "president", "press", "pressure",
     "pretty", "prevent", "previous", "price", "pride", "primitive", "principal", "principle",
     "printed", "private", "prize", "probably", "problem", "process", "produce", "product",
     "production", "program", "progress", "promised", "proper", "properly", "property", "protection",
     "proud", "prove", "provide", "public", "pull", "pupil", "pure", "purple",
     "purpose", "push", "put", "putting", "quarter", "queen", "question", "quick",
     "quickly", "quiet", "quietly", "quite", "rabbit", "race", "radio", "railroad",
     "rain", "raise", "ran", "ranch", "range", "rapidly", "rate", "rather",
     "raw", "rays", "reach", "read", "reader", "ready", "real", "realize",
     "rear", "reason", "recall", "receive", "recent", "recently", "recognize", "record",
     "red", "refer", "refused", "region", "regular", "related", "relationship", "religious",
     "remain", "remarkable", "remember", "remove", "repeat", "replace", "replied", "report",
     "represent", "require", "research", "respect", "rest", "result", "return", "review",
     "rhyme", "rhythm", "rice", "rich", "ride", "riding", "right", "ring",
     "rise", "rising", "river", "road", "roar", "rock", "rocket", "rocky",
     "rod", "roll", "roof", "room", "root", "rope", "rose", "rough",
     "round", "route", "row", "rubbed", "rubber", "rule", "ruler", "run",
     "running", "rush", "sad", "saddle", "safe", "safety", "said", "sail",
     "sale", "salmon", "salt", "same", "sand", "sang", "sat", "satellites",
     "satisfied", "save", "saved", "saw", "say", "scale", "scared", "scene",
     "school", "science", "scientific", "scientist", "score", "screen", "sea", "search",
     "season", "seat", "second", "secret", "section", "see", "seed", "seeing",
     "seems", "seen", "seldom", "select", "selection", "sell", "send", "sense",
     "sent", "sentence", "separate", "series", "serious", "serve", "service", "sets",
     "setting", "settle", "settlers", "seven", "several", "shade", "shadow", "shake",
     "shaking", "shall", "shallow", "shape", "share", "sharp", "she", "sheep",
     "sheet", "shelf", "shells", "shelter", "shine", "shinning", "ship", "shirt",
     "shoe", "shoot", "shop", "shore", "short", "shorter", "shot", "should",
     "shoulder", "shout", "show", "shown", "shut", "sick", "sides", "sight",
     "sign", "signal", "silence", "silent", "silk", "silly", "silver", "similar",
     "simple", "simplest", "simply", "since", "sing", "single", "sink", "sister",
     "sit", "sitting", "situation", "six", "size", "skill", "skin", "sky",
     "slabs", "slave", "sleep", "slept", "slide", "slight", "slightly", "slip",
     "slipped", "slope", "slow", "slowly", "small", "smaller", "smallest", "smell",
     "smile", "smoke", "smooth", "snake", "snow", "so", "soap", "social",
     "society", "soft", "softly", "soil", "solar", "sold", "soldier", "solid",
     "solution", "solve", "some", "somebody", "somehow", "someone", "something", "sometime",
     "somewhere", "son", "song", "soon", "sort", "sound", "source", "south",
     "southern", "space", "speak", "special", "species", "specific", "speech", "speed",
     "spell", "spend", "spent", "spider", "spin", "spirit", "spite", "split",
     "spoken", "sport", "spread", "spring", "square", "stage", "stairs", "stand",
     "standard", "star", "stared", "start", "state", "statement", "station", "stay",
     "steady", "steam", "steel", "steep", "stems", "step", "stepped", "stick",
     "stiff", "still", "stock", "stomach", "stone", "stood", "stop", "stopped",
     "store", "storm", "story", "stove", "straight", "strange", "stranger", "straw",
     "stream", "street", "strength", "stretch", "strike", "string", "strip", "strong",
     "stronger", "struck", "structure", "struggle", "stuck", "student", "studied", "studying",
     "subject", "substance", "success", "successful", "such", "sudden", "suddenly", "sugar",
     "suggest", "suit", "sum", "summer", "sun", "sunlight", "supper", "supply",
     "support", "suppose", "sure", "surface", "surprise", "surrounded", "swam", "sweet",
     "swept", "swim", "swimming", "swing", "swung", "syllable", "symbol", "system",
     "table", "tail", "take", "taken", "tales", "talk", "tall", "tank",
     "tape", "task", "taste", "taught", "tax", "tea", "teach", "teacher",
     "team", "tears", "teeth", "telephone", "television", "tell", "temperature", "ten",
     "tent", "term", "terrible", "test", "than", "thank", "that", "thee",
     "them", "themselves", "then", "theory", "there", "therefore", "these", "they",
     "thick", "thin", "thing", "think", "third", "thirty", "this", "those",
     "thou", "though", "thought", "thousand", "thread", "three", "threw", "throat",
     "through", "throughout", "throw", "thrown", "thumb", "thus", "thy", "tide",
     "tie", "tight", "tightly", "till", "time", "tin", "tiny", "tip",
     "tired", "title", "to", "tobacco", "today", "together", "told", "tomorrow",
     "tone", "tongue", "tonight", "too", "took", "tool", "top", "topic",
     "torn", "total", "touch", "toward", "tower", "town", "toy", "trace",
     "track", "trade", "traffic", "trail", "train", "transportation", "trap", "travel",
     "treated", "tree", "triangle", "tribe", "trick", "tried", "trip", "troops",
     "tropical", "trouble", "truck", "trunk", "truth", "try", "tube", "tune",
     "turn", "twelve", "twenty", "twice", "two", "type", "typical", "uncle",
     "under", "underline", "understanding", "unhappy", "union", "unit", "universe", "unknown",
     "unless", "until", "unusual", "up", "upon", "upper", "upward", "us",
     "use", "useful", "using", "usual", "usually", "valley", "valuable", "value",
     "vapor", "variety", "various", "vast", "vegetable", "verb", "vertical", "very",
     "vessels", "victory", "view", "village", "visit", "visitor", "voice", "volume",
     "vote", "vowel", "voyage", "wagon", "wait", "walk", "wall", "want",
     "war", "warm", "warn", "was", "wash", "waste", "watch", "water",
     "wave", "way", "we", "weak", "wealth", "wear", "weather", "week",
     "weigh", "weight", "welcome", "well", "went", "were", "west", "western",
     "wet", "whale", "what", "whatever", "wheat", "wheel", "when", "whenever",
     "where", "wherever", "whether", "which", "while", "whispered", "whistle", "white",
     "who", "whole", "whom", "whose", "why", "wide", "widely", "wife",
     "wild", "will", "willing", "win", "wind", "window", "wing", "winter",
     "wire", "wise", "wish", "with", "within", "without", "wolf", "women",
     "won", "wonder", "wonderful", "wood", "wooden", "wool", "word", "wore",
     "work", "worker", "world", "worried", "worry", "worse", "worth", "would",
     "wrapped", "write", "writer", "writing", "written", "wrong", "wrote", "yard",
     "year", "yellow", "yes", "yesterday", "yet", "you", "young", "younger",
     "your", "yourself", "youth", "zero", "zoo"
];
var easyDictionary = ["ll", "mm", "pp", "ii", "cc", "xx", "ee", "ss", "oo", "aa", "ff", "uu", "dd", "bb", "tt", "vv", "all", "see", "off", "too", "add", "www", "fee", "ill", "egg", "odd", "bee", "app", "iii", "inn", "tee", "zoo", "wee", "dll", "err", "foo", "cpp", "woo", "hmm", "hee", "ebb", "soo", "gcc", "goo", "att", "lee", "css", "nee", "doo", "vii", "arr", "iff", "hoo", "hii", "gee", "boo", "mmm", "umm", "ell", "acc", "mee", "ooo", "uhh", "been", "good", "need", "book", "look", "keep", "took", "food", "seen", "room", "feel", "week", "soon", "meet", "feet", "seem", "door", "deep", "poor", "http", "tool", "cool", "wood", "foot", "seek", "pool", "moon", "fees", "root", "feed", "adds", "sees", "seed", "beer", "eggs", "roof", "cook", "loop", "mood", "beef", "deer", "boot", "odds", "peer", "hook", "keen", "noon", "fool", "wool", "reef", "boom", "hood", "bees", "deed", "ally", "weed", "zoom", "teen", "doom", "heel", "reel", "peel", "apps", "jeep", "inns", "heed", "peek", "reed", "geek", "ammo", "deem", "loom", "loot", "weep", "ills", "boon", "hoop", "moot", "keel", "alle", "seer", "beep", "beet", "zoos", "hoof", "esse", "meek", "tees", "peep", "alla", "soot", "voor", "coop", "nook", "rook", "seep", "hmmm", "moor", "hoot", "eddy", "veer", "neem", "viii", "doon", "maar", "ello", "offs", "meer", "naar", "addr", "leek", "ella", "leer", "goof, needs", "heels", "hooks", "alley", "beers", "reefs", "alloy", "goose", "roofs", "oddly", "needy", "moose", "fools", "attic", "geese", "cooks", "moods", "moons", "annum", "reels", "looms", "woody", "reeds", "erred", "otter", "deems", "geeks", "abbey", "beech", "annex", "hoops", "goofy", "booty", "abbot", "annoy", "asses", "moody", "roost", "cools", "hoods", "booze", "httpd", "https", "jeeps", "ellos", "leech", "booms", "illus", "roomy", "beets", "soooo", "noose", "noone", "peels", "allot", "seers", "peeps", "zooms", "affix", "seedy", "sooty", "allay", "upped", "moors", "booby", "seeps", "weedy", "adder", "beeps", "allen", "goons", "geeky", "alles", "udder", "leery", "nooks", "loons", "leeks", "gooey", "access", "office", "issues", "effect", "offers", "needed", "allows", "appear", "attack", "effort", "looked", "annual", "seemed", "accept", "seeing", "issued", "assist", "affect", "adding", "appeal", "occurs", "errors", "attend", "weekly", "assume", "assets", "arrive", "immune", "deeply", "wooden", "assess", "afford", "arrest", "essays", "deeper", "allies", "errant", "noodle", "errata", "looped", "woolen", "weevil", "zoomed", "poodle", "errand", "loomed", "efflux", "goodly", "leeway", "woolly", "looser", "heeded", "assail", "effigy", "oddity", "boogie", "allude", "eddies", "roofed", "coolly", "accede", "looses", "deeded", "affine", "boomed", "hookup", "veered", "utters", "annoys", "loosed", "footed", "boomer", "reeled", "meetup", "looking", "address", "effects", "meeting", "account", "efforts", "allowed", "appears", "offered", "applied", "attempt", "feeling", "officer", "attacks", "keeping", "arrived", "affairs", "seeking", "weekend", "offices", "illegal", "illness", "opposed", "arrival", "accused", "assumed", "applies", "cooking", "alleged", "feeding", "essence", "assault", "affects", "attract", "appeals", "booking", "arrange", "assured", "cookies", "cooling", "assumes", "offense", "accepts", "arrives", "approve", "immense", "footage", "utterly", "offence", "booklet", "assists", "needing", "issuing", "apparel", "teenage", "foolish", "toolbar", "arrests", "deepest", "allergy", "appoint", "offline", "seekers", "asserts", "loosely", "doorway", "needles", "goodbye", "toolkit", "innings", "illicit", "poorest", "booster", "goodies", "accents", "seeming", "ammonia", "lookout", "annoyed", "acclaim", "assures", "beetles", "assigns", "applets", "uttered", "alleles", "unnamed", "footing", "opposes", "keepers", "immoral", "affords", "weeping", "addicts", "looting", "alleges", "attends", "annuity", "accords", "coolest", "booming", "roofing", "annexed", "erratic", "noodles", "feeders", "looming", "toolbox", "pooling", "occured", "boosted", "weekday", "boolean", "seeding", "accrued", "applaud", "tooling", "boomers", "accuses", "affirms", "peering", "alluded", "affixed", "coolant", "attains", "arrears", "looping", "offsets", "peeling", "issuers", "rooting", "rooftop", "coolers", "attuned", "immerse", "appease", "teeming", "loosing", "assayed", "reeling", "booting", "hooking", "mooring", "zooming", "rooster", "fooling", "bootleg", "ellipse", "reentry", "errands", "cookery", "weeding", "offbeat", "arrayed", "alludes", "zoology", "lookups", "leeward", "annuals", "woollen", "offsite", "affront", "looters", "oppress", "attests", "accrual", "deepens", "allelic", "woodcut", "reenter", "accuser", "annular", "beehive", "toolset", "adduced", "rookies", "woodland", "imminent", "attacker", "applause", "attracts", "teenager", "afforded", "occupies", "effected", "footnote", "offender", "allocate", "offenses", "soothing", "goodwill", "offences", "assorted", "appendix", "issuance", "affirmed", "offended", "arrogant", "immature", "addicted", "immersed", "assaults", "additive", "accusing", "footwear", "cookbook", "arrivals", "allotted", "assuring", "attaches", "accorded", "alleging", "assassin", "affluent", "appended", "accesses", "approves", "effluent", "boosting", "roommate", "doorstep", "booklets", "needless", "occupant", "weekdays", "essences", "assesses", "ammonium", "attested", "accented", "seedling", "arranges", "appalled", "allergen", "illusory", "allegory", "boosters", "allusion", "bookshop", "effector", "deepened", "foothold", "elliptic", "occuring", "woodwork", "doorways", "addendum", "footpath", "alluvial", "innovate", "appoints", "arranger", "loosened", "eggplant", "doomsday", "assessor", "afferent", "loophole", "coolness", "uttering", "moorings", "offshoot", "alluring", "irritate", "toolbars", "immanent", "irritant", "footwork", "immobile", "rooftops", "acceptor", "oddities", "woodwind", "annotate", "attendee", "keepsake", "seething", "cookware", "alluding", "assailed", "poolside", "bootable", "occupier", "toolkits", "appraise", "appointed", "assistant", "addressed", "associate", "attempted", "attribute", "attending", "attitudes", "occasions", "affecting", "offerings", "offensive", "applicant", "occurring", "appearing", "opponents", "attracted", "affiliate", "attorneys", "additions", "assembled", "seemingly", "accepting", "accidents", "allegedly", "attacking", "assurance", "assessing", "apparatus", "allocated", "assisting", "accompany", "accessing", "aggregate", "addiction", "illnesses", "appealing", "offspring", "bookstore", "cooperate", "immigrant", "accounted", "offenders", "irregular", "acclaimed", "assertion", "teenagers", "appraisal", "affection", "attendees", "announces", "innocence", "allowance", "alliances", "annotated", "occupying", "illegally", "accession", "accessory", "allergies", "arranging", "bookmarks", "appliance", "appellant", "attendant", "footsteps", "oppressed", "occupancy", "alleviate", "addictive", "appellate", "erroneous", "seedlings", "immensely", "assigning", "eccentric", "occupants", "illusions", "immersion", "attackers", "affidavit", "arrogance", "offending", "foothills", "additives", "attaining", "attentive", "afflicted", "attaching", "asserting", "unnatural", "footprint", "footnotes", "allowable", "unnoticed", "assaulted", "accordion", "affective", "moonlight", "woodlands", "approving", "appalling", "arresting", "irritated", "assistive", "utterance", "irrigated", "opposites", "annoyance", "approvals", "deepening", "assembler", "assassins", "occlusion", "allergens", "irritable", "affirming", "applauded", "attrition", "alligator", "announcer", "innovator", "immediacy", "effecting", "allotment", "accolades", "assertive", "appraiser", "bookshelf", "allusions", "immutable", "innermost", "illogical", "annuities", "innocuous", "apprehend", "cookbooks", "accretion", "innocents", "immortals", "bootstrap", "uppermost", "annealing", "loopholes", "loosening", "aggressor", "affording", "boomerang", "appraised", "allocates", "immersive", "immovable", "immunized", "appetites", "assuredly", "appetizer", "aggravate", "arrowhead", "uppercase", "assessors", "aggrieved", "assembles", "assailant", "affluence", "foolishly", "roommates", "occupiers", "illegible", "foodborne", "foolproof", "footpaths", "allograft", "afferents", "opportune", "addressee", "innkeeper", "oppressor", "occultism", "bookshops", "unnerving", "effluents", "attractor", "appointee", "appendage", "occipital", "deepwater", "annulment", "occurence", "toothpick", "reelected", "moonshine", "irritants", "attesting", "offseason", "officiate", "effectors", "reexamine", "immigrate", "attenuate", "appending", "toothless", "effectual", "immersing", "immensity", "foolhardy", "feedstock", "additional", "associated", "assistance", "assessment", "apparently", "appearance", "opposition", "approaches", "accessible", "efficiency", "innovative", "appreciate", "applicable", "accordance", "accounting", "attractive", "acceptable", "attributes", "acceptance", "occupation", "innovation", "affordable", "assignment", "aggressive", "attempting", "officially", "occasional", "addressing", "assumption", "accurately", "approached", "applicants", "accomplish", "attachment", "attributed", "attendance", "immigrants", "affiliated", "illustrate", "coordinate", "occurrence", "attraction", "allocation", "irrigation", "associates", "aggression", "ammunition", "appliances", "irrelevant", "accredited", "affiliates", "assistants", "oppression", "accelerate", "accustomed", "accidental", "attracting", "announcing", "assemblies", "assortment", "allegiance", "accumulate", "essentials", "attainment", "irrational", "irritation", "assertions", "accountant", "apprentice", "bookstores", "annotation", "assembling", "oppressive", "allegation", "aggregates", "assurances", "attendants", "accusation", "illuminate", "aggravated", "irritating", "footprints", "allowances", "illiterate", "assemblage", "immunoblot", "bookkeeper", "appraising", "footbridge", "allergenic", "accessable", "affordably", "immaturity", "allopathic", "announcers", "annualized", "applauding", "attractors", "immobility", "woodpecker", "innkeepers", "application", "appropriate", "opportunity", "immediately", "association", "effectively", "cooperation", "essentially", "accompanied", "accessories", "appointment", "arrangement", "illustrated", "immigration", "anniversary", "accordingly", "cooperative", "appreciated", "accommodate", "attractions", "assumptions", "assignments", "coordinator", "approaching", "efficiently", "unnecessary", "allegations", "assessments", "illustrates", "appearances", "coordinates", "coordinated", "innovations", "approximate", "accumulated", "attachments", "accelerated", "affirmative", "accountable", "affiliation", "accusations", "occupations", "illuminated", "occurrences", "aggregation", "immortality", "innumerable", "cooperating", "accelerator", "accountants", "irradiation", "accompanies", "abbreviated", "annotations", "illustrator", "affirmation", "attribution", "illustrious", "allocations", "assimilated", "attenuation", "woodworking", "associative", "appreciates", "apprehended", "booksellers", "erroneously", "associating", "officiating", "accelerates", "accumulates", "assemblages", "apprentices", "assassinate", "appreciable", "illuminates", "allegorical", "bookkeeping", "efficacious", "immobilized", "foodservice", "annihilated", "arrhythmias", "irrevocable", "alleviation", "aggravating", "accountancy", "irregularly", "attributing", "aggravation", "appellation", "accentuated", "alleviating", "irreparable", "foolishness", "reestablish", "zooplankton", "irreducible", "accomplices", "loosestrife", "apprenticed", "afflictions", "immunologic", "accrediting", "attentively", "irrevocably", "attentional", "apparitions", "appreciably", "innervation", "appeasement", "aggregating", "irrefutable", "oppositions", "bookshelves", "accumulator", "opportunism", "approbation", "woodpeckers", "immunoassay", "apportioned", "accompanist", "offensively", "attainments", "allegiances", "opportunist", "attitudinal", "occultation", "irrelevance", "irritations", "applicators", "reenactment", "apparatuses", "unnaturally", "applications", "arrangements", "occasionally", "accomplished", "associations", "announcement", "coordination", "appreciation", "occupational", "accompanying", "illustration", "accumulation", "appointments", "acceleration", "accidentally", "coordinating", "illumination", "aggressively", "attributable", "irrespective", "illustrating", "assimilation", "immunization", "accelerating", "efficiencies", "irreversible", "assassinated", "apprehension", "accommodated", "appropriated", "coordinators", "abbreviation", "irresistible", "illuminating", "additionally", "illegitimate", "cooperatives", "opportunities", "approximately", "accommodation", "effectiveness", "illustrations", "announcements", "accessibility", "appropriately", "assassination", "accreditation", "approximation", "accompaniment", "abbreviations", "applicability", "irresponsible", "appropriation", "unnecessarily", "accommodating", "accomplishing", "opportunistic", "immunological", "anniversaries", "cooperatively", "acceptability", "affordability", "accommodations", "accountability", "accomplishment", "appropriations", "ecclesiastical", "apprenticeship", "irregularities", "immunoglobulin", "affectionately", "attractiveness", "assassinations", "approximations", "aggressiveness", "immobilization", "irreconcilable", "immunoblotting", "immunostaining", "immunoreactive", "accompaniments", "supplementation", "dissatisfaction", "differentiating", "correspondingly", "correspondences", "collaboratively", "surreptitiously", "immunodeficiency", "immunoreactivity", "irresponsibility", "crystallographic", "programmatically"];

function subNum(temp) {
     temp = temp.replace(/o/g, '0');
     temp = temp.replace(/e/g, '3');
     temp = temp.replace(/l/g, '1');
     temp = temp.replace(/s/g, '5');
     return temp;
}

function generatePassword() {
     var minWLength = document.querySelector('#minWL').value;
     var maxWLength = document.querySelector('#maxWL').value;
     var maxLength = document.querySelector('#maxLength').value;
     var easyTyping = document.querySelector('#easyTyping').checked;
     var numberSubstitutions = document.querySelector('#numberSubstitutions').checked;
     var wordList = dictionary;
     if (easyTyping) {
          wordList = easyDictionary;
     }
     var numWords = wordList.length;
     var word;
     var wordLength;
     var passLength = 0;
     var tbl = document.querySelector("#historytable"); //select table]
     tbl.innerHTML = " ";
     var previousPass = [];
     var currentPass = [];

     /////////////////////////////////////////////////
     for (var count = 1; count < numWords; count++) {
          for (var j = count; j < numWords; j++) {
               word = wordList[j];
               wordLength = word.length;

               //found the valid word
               if (wordLength <= maxWLength && wordLength >= minWLength && wordLength <= maxLength) {
                    passLength += wordLength;
                    maxLength -= wordLength;
                    if (numberSubstitutions)
                         word = subNum(word);
                    currentPass.push(word);
               }
          }

          if (JSON.stringify(currentPass) != JSON.stringify(previousPass)) {
               //alert ("Current: "+currentPass + " " + "Previous: "+previousPass);
               previousPass = currentPass;
               if (passLength > 0) {
                    var tr = document.createElement("tr"); //create a row
                    tbl.appendChild(tr); //add row to table
                    var tw = document.createElement("tw"); //create a column
                    tr.appendChild(tw); //add column to table
                    var td = document.createElement("td"); //create data
                    for (var i = 0; i < previousPass.length; i++)
                         td.innerHTML += previousPass[i] + " ";
                    td.innerHTML += passLength;
                    tw.appendChild(td);
               }
          }
          currentPass = [];
          passLength = 0;
          maxLength = document.querySelector('#maxLength').value;
     }
     //word = dictionary[Math.floor(Math.random()*numWords)];
}
