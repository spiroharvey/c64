

 "" #  53280,0 : 53281,0 0 A$²"ÕÃÉ" = B$²"Â Â" J  C$²"ÊÃË" ]! P$²"Ú" : AT²0 j#  GA$(5) }(  NW: WD$(NW) - DS²Â(66)¬256ªÂ(65)ª6 ¼2 £15)"W O R D L E         V1.2" ì3 "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" 	7 " C=64 CONVERSION BY SPIRO HARVEY, 2022" D	8 "         ORIGINAL BY JOSH WARDLE" p	9 "    SPECIAL THANKS TO DAN SANDERSON"  	; "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" ¹	<  2000 :  LOAD DATA ø	> "                                " 
A £15);"HOW TO PLAY" ;
F " GUESS THE WORDLE IN 6 TRIES." h
P " AFTER EACH GUESS, THE COLOUR OF THE" 
Z " TILES WILL CHANGE TO SHOW HOW CLOSE" ¸
d " YOUR GUESS WAS TO THE WORD." è
n "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" o " PRESS RETURN TO START; SPACE FOR HELP" 2p ¡ K$: K$²"" § 112 Iq  K$²Ç(13) §  300 r  K$²" " § "                                       ": 120 s  112 µx "    ";A$;"";A$;A$;A$;A$ Ò} "    ÂWÂÂEÂÂAÂÂRÂÂYÂ" ò "    ";C$;"";C$;C$;C$;C$   " THE LETTER W IS IN THE WORD AND IN" : " THE CORRECT SPOT." ^ "    ";A$;A$;"";A$;"";A$;A$ | "    ÂPÂÂIÂÂLÂÂOÂÂTÂ"    "    ";C$;C$;"";C$;"";C$;C$ Ï¥ " THE LETTER L IS IN THE WORD, BUT AT" ãª " WRONG SPOT." í´  250 È "    ";A$;A$;A$;"";A$;"";A$ 9Í "    ÂVÂÂAÂÂGÂÂUÂÂEÂ          " ]Ò "    ";C$;C$;C$;"";C$;"";C$ × " THE LETTER U IS NOT IN THE WORD." Ü  250 æ  300 Âú  ------------- SCREEN PAUSE --- ñÿ "        PRESS ENTER TO CONTINUE..." 	¡ K$: K$²"" § 9010  	 K$³±Ç(13) § 9010 Q"                                      " W |, -------------- GAME SCREEN --- 1R²µ(»(1)¬NW) 6PW$²WD$(R) ¥;"" ½@£15)"W O R D L E" ÆE"" ÔJ C²1 ¤ 6 íO£13);A$;A$;A$;A$;A$ T£13);B$;B$;B$;B$;B$ Y£13);C$;C$;C$;C$;C$ %^ J ---------------- MAIN LOOP --- i AT²1 ¤ 6 :  ATTEMPT NUM  I²1 ¤ 5:GA$(I)²"": GS$²"" ¨¤ 680 :  UPDATE LINE Ã® 660 :  HILIGHT LINE Ü¸ 600 :  READ GUESS âÂ Ì ----------------- END GAME --- 0Ö£7);"SORRY, THE WORD WAS "PW$"." :à 530 Mô --- YOU WIN! mþ""; £12);"HUZZAH!! YOU WIN!" ¹"WOULD YOU LIKE TO PLAY AGAIN? (Y/N)"; Â¡ K$ Ó& K$²"N" §  è0 K$²"Y" §  300 ò: 540 X --------------- READ GUESS --- .]C²0 :  CHAR COUNT Gb¡ K$ :  K$²"" § 610 dl K$²Ç(20) ¯ C±0 §  700 v K$²Ç(13) ¯ C±²4 §  800:  ¥ K$±²"A" ¯ K$³²"Z" §  750 ¯ 610 µ Ú -------- HILIGHT CURR LINE --- ó 1024ª(LN¬40)ª11,90  55296ª(LN¬40)ª11,10 £ 8¨ -------------- UPDATE LINE --- Q­ 1024ª(LN¬40)ª11,32 a²LN²(AT¬3)ª1 g· ¼ ---------------- DEL CHAR --- ÁX²C«1 ¦ÆGS$²È(GS$,X) ÅË 1024ª(LN¬40)ª(X¬3)ª14,32 ÍÐC²X æÕ 610 :  NEXT GUESS 
î ------------- ACCEPT CHAR --- ó C±²5 §  610 ,øGS$²GS$ªK$ Qý 1024ª(LN¬40)ª(C¬3)ª14,Æ(K$)«64 [C²Cª1 e 610   ---------------- FIND WORD --- ©! FIND GS$ IN DATA QUICKLY Ð" R = RESULT:-1 IF FOUND, 0 IF NOT # EXPECTS NW=# OF WORDS, DS=ADDR OF FIRST WORD $ GS$²PW$ §  950 /%SC$²"ÄÍÂÎ" : SC²1 >(LO²0:HI²NW T*I²µ((HI«LO)­2)ªLO ^/ 850 h1 880 z2 SC²5 § SC²1 4 GS$²W$ § R²«1: 950 §9 GS$³W$ § 840 ¾> LO²I § R²0: 900 ÍCLO²I: 810 äH HI²I § R²0: 900 óMHI²I: 810 R --- SET W$ TO THE I-TH WORD --- <WIA²DSª36¬µ(I­5)ª(I«µ(I­5)¬5)¬6 W\ I²µ(I­5)¬5 § IA²IA«6 a 66,µ(IA­256): 65,IA«(µ(IA­256)¬256) f W$ k £p --- SPINNER ­r""; ·uZM²«1 Òz£39);"";Ê(SC$,SC,1); ÞSC²SCª1 ä ÿ --- WORD NOT IN DICT "";£39)" " 0""; S£10)"WORD NOT IN DICTIONARY!" v£10)"PRESS A KEY TO CONTINUE" ¡ K$ :  K$²"" §  920 ±""; Ô¢£10)"                       " ÷§£10)"                       " ¬ 610 ¶ --- SOLVE "·"";£39)" " 0» Z²1 ¤ 5 ?À ZB²1 ¤ 5 iÅ Ê(GS$,Z,1)²Ê(PW$,ZB,1) § GA$(Z)²"-" rÊ ZB Ï Ê(GS$,Z,1)²Ê(PW$,Z,1) § GA$(Z)²"+" £Ô Z »è --- SOLVE COLOURS Éí Z²1 ¤ 5 éò GA$(Z)²"+" § CL²13: 1100 ü GA$(Z)²"-" § CL²9: 1100 ( GA$(Z)²""  § CL²11: 1100 . D GS$²PW$ §  500 N$ 655 rL ----------- HILIGHT BOXES --- Q C²1 ¤ 3 ¥V 55296ª((LN«1)¬40)ª(Z¬3)ª9ªC,CL «[ ¹` C²1 ¤ 3 Úe 55296ª(LN¬40)ª(Z¬3)ª9ªC,CL àj îo C²1 ¤ 3 t 55296ª((LNª1)¬40)ª(Z¬3)ª9ªC,CL y ~ CÐ ------------- DATA LOADER --- bÚ£12)"LOADING DATA..."; jäC²1 yî W²0 ¤ NW ø WD$(W) ý C²1 § "..."; ¹ C²2 § "..."; Ò C²3 § "..."; â C²4 § C²0 ìC²Cª1 ò ø3 ' 1510 '' ABBEY,ABBOT,ABHOR,ABIDE,ABODE K' ABORT,ABOUT,ABOVE,ABYSS,ACORN o' ACTOR,ADEPT,ADMIN,ADOPT,ADORE ' ADORN,ADULT,AFIRE,AFOOT,AFOUL ·$' AFTER,AGAIN,AGONY,AGREE,AHEAD Û)' AIDED,AISLE,ALARM,ALBUM,ALIEN ÿ.' ALIGN,ALLOW,ALOFT,ALOHA,AMAZE #3' AMUSE,ANGEL,ANVIL,APPLE,ARGON G8' ARSON,AUDIO,AUDIT,AVOID,AWAKE k=' BACON,BADGE,BAGEL,BAGGY,BAKER B' BALLS,BANJO,BARGE,BASIC,BASIN ³G' BASIS,BATCH,BAWDY,BEACH,BEAST ×L' BEATS,BEGAN,BEGAT,BELCH,BELLY ûQ' BELOW,BERET,BERRY,BEVVY,BIGOT V' BIKER,BIMBO,BINGE,BLACK,BLADE C[' BLAME,BLAND,BLANK,BLAST,BLAZE g`' BLOOD,BLOOM,BLOWN,BLUFF,BLUNT e' BLURB,BLURT,BLUSH,BOARD,BOGUS ¯j' BONER,BOOBS,BOOTH,BORED,BOSSY Óo' BOUND,BRACE,BRAID,BRAIN,BRAKE ÷t' BRAND,BRASH,BRASS,BRAVE,BRAWL y' BREAD,BREAK,BRICK,BRIDE,BRIEF ?~' BROOK,BRUTE,BUGGY,BULGE,BUNCH c' BUNNY,BURLY,BUTCH,BUXOM,BYLAW ' CABAL,CABIN,CABLE,CACHE,CADET «' CAMEL,CANAL,CANDY,CANOE,CANON Ï' CAPER,CARAT,CARRY,CARVE,CATCH ó' CATER,CAULK,CAUSE,CEASE,CEDAR ' CELLO,CHAIR,CHALK,CHAMP,CHANT ;¡' CHAOS,CHARM,CHASE,CHASM,CHEAP _¦' CHEAT,CHECK,CHEEK,CHEER,CHICK «' CHIEF,CHILD,CHIME,CHIRP,CHOKE §°' CHORD,CHORE,CHUNK,CHUTE,CIVIC Ëµ' CIVIL,CLAMP,CLASH,CLASP,CLASS ïº' CLAWS,CLEAN,CLEAR,CLICK,CLIFF ¿' CLIMB,CLOAK,CLOCK,CLONE,CLOTH 7Ä' CLOUD,CLOWN,CLUMP,COACH,COAST [É' COBRA,CODEX,COLON,COMIC,COPSE Î' CORAL,COUCH,COUGH,COULD,COUNT £Ó' CRACK,CRAFT,CRAMP,CRANE,CRANK ÇØ' CRASH,CRASS,CRATE,CRAVE,CRAWL ëÝ' CREAM,CREEK,CREEP,CRIME,CRISP  â' CROAK,CROFT,CROSS,CROWD,CROWN 3 ç' CRUDE,CRUEL,CRUMB,CRUSH,CRUST W ì' CRYPT,CURIO,CYCLE,CYNIC,DAILY { ñ' DAIRY,DAISY,DANCE,DANDY,DATUM  ö' DAUNT,DEATH,DEBIT,DEBUG,DEBUT Ã û' DECAL,DECAY,DECOR,DECOY,DELAY ç  ( DELTA,DENIM,DENSE,DEPOT,DEPTH !( DEVIL,DIARY,DIGIT,DILDO,DINER /!
( DIRGE,DIRTY,DISCO,DITCH,DITTO S!( DODGE,DOGMA,DONOR,DOUBT,DOUGH w!( DOUSE,DOZEN,DRAFT,DRAIN,DRAMA !( DRANK,DRAPE,DRAWL,DRAWN,DREAD ¿!( DRIFT,DRILL,DRINK,DRIVE,DRONE ã!#( DROOL,DROOP,DROVE,DRUID,DWARF "(( DWELL,EAGER,EAGLE,EARLY,EARTH +"-( EASEL,EBONY,EDICT,EDIFY,EERIE O"2( EIGHT,EJECT,ELBOW,ELDER,ELECT s"7( ELITE,EMAIL,EMBED,EMBER,EMPTY "<( ENACT,ENEMA,ENEMY,ENJOY,ENTER »"A( ENTRY,ENVOY,EPOCH,EPOXY,EQUAL ß"F( EQUIP,ERASE,ERECT,ERROR,ERUPT #K( ESSAY,EVADE,EVENT,EVICT,EVOKE '#P( EXACT,EXILE,EXIST,FABLE,FACET K#U( FAINT,FAIRY,FALSE,FANCY,FANNY o#Z( FATAL,FEAST,FEIGN,FELON,FENCE #_( FERAL,FERRY,FETCH,FEVER,FEWER ·#d( FIGHT,FILTH,FINAL,FLAIL,FLAIR Û#i( FLAKE,FLAME,FLANK,FLARE,FLASH ÿ#n( FLASK,FLEET,FLESH,FLINT,FLIRT #$s( FLOAT,FLOCK,FLOOD,FLOOR,FLOUR G$x( FLUFF,FLUID,FLUKE,FLUSH,FLUTE k$}( FOCUS,FOLIO,FORAY,FORCE,FORGE $( FORGO,FORTH,FOUND,FRAIL,FRAME ³$( FRAUD,FREAK,FRESH,FRILL,FRISK ×$( FROCK,FROST,FROTH,FROWN,FROZE û$( FRUIT,FUDGE,FUNGI,FUSSY,FUTON %( GABLE,GAMMA,GAMUT,GAUDY,GAUGE C%( GAUNT,GAVEL,GEESE,GENIE,GENRE g% ( GHOST,GHOUL,GIANT,GIZMO,GLADE %¥( GLAND,GLASS,GLAZE,GLEAM,GLEAN ¯%ª( GLINT,GLOAT,GLOBE,GLOOM,GLOSS Ó%¯( GLOVE,GLYPH,GNASH,GNOME,GOOSE ÷%´( GRAFT,GRAIL,GRAIN,GRAND,GRAPH &¹( GRASP,GRASS,GRATE,GRAVE,GRAVY ?&¾( GRAZE,GREAT,GREED,GREEN,GREET c&Ã( GRIEF,GRILL,GRIME,GRIND,GROAN &È( GROOM,GRUNT,GUARD,GUESS,GUEST «&Í( GUIDE,GUILD,GUILE,GUILT,GUISE Ï&Ò( GYPSY,HABIT,HAIKU,HAIRY,HAPPY ó&×( HARDY,HAREM,HARPY,HARSH,HASTE 'Ü( HASTY,HAUNT,HAVEN,HAVOC,HEADY ;'á( HEARD,HEART,HEAVE,HEAVY,HEDGE _'æ( HEFTY,HEIST,HIKER,HINGE,HIPPO 'ë( HIPPY,HITCH,HOARD,HOBBY,HOIST §'ð( HONEY,HORDE,HORNS,HORNY,HORSE Ë'õ( HOTEL,HOUND,HOUSE,HOVEL,HOVER ï'ú( HOWDY,HUMAN,HUMID,HUNCH,HURRY (ÿ( HYDRO,HYENA,IDEAL,IDIOM,IDIOT 7() IGLOO,IMAGE,IMBUE,IMPEL,IMPLY [(	) INANE,INCUR,INDEX,INDIE,INEPT () INERT,INFER,INGOT,INKED,INLAY £() INLET,INNER,INSET,IRATE,IRONY Ç() ISSUE,IVORY,JADED,JAUNT,JEANS ë() JELLY,JETTY,JEWEL,JIFFY,JOINT )") JOIST,JOKER,JOLLY,JOULE,JOUST 3)') JOWLS,JUDGE,JUICE,JUMBO,JUROR W),) KANJI,KARAT,KARMA,KAYAK,KEBAB {)1) KHAKI,KINKY,KIOSK,KLUTZ,KNACK )6) KNAVE,KNEAD,KNEED,KNEEL,KNELL Ã);) KNIFE,KNOCK,KNOLL,KNOWN,KOALA ç)@) KRONE,KUDOS,LABEL,LABIA,LACES *E) LADEN,LADLE,LAGER,LAIRD,LAMER /*J) LANCE,LANKY,LAPEL,LAPSE,LARGE S*O) LARVA,LASER,LATCH,LATER,LATEX w*T) LATHE,LATTE,LAUGH,LAYER,LEAKY *Y) LEARN,LEASE,LEASH,LEAST,LEAVE ¿*^) LEDGE,LEECH,LEERY,LEGAL,LEGIT ã*c) LEMON,LEMUR,LEPER,LEVEE,LEVEL +h) LEVER,LIBEL,LIEGE,LIGHT,LILAC ++m) LIMBO,LIMIT,LINEN,LINGO,LITHE O+r) LIVEN,LIVER,LIVID,LLAMA,LOAMY s+w) LOATH,LOBBY,LOCAL,LOCUS,LODGE +|) LOGIC,LOLLY,LONER,LOOSE,LORRY »+) LOSER,LOTUS,LOUSE,LOVER,LOWER ß+) LOWLY,LOYAL,LUCID,LUCKY,LUMEN ,) LUNAR,LUNCH,LUNGE,LUPUS,LURCH ',) LURID,LYNCH,LYRIC,MACHO,MACRO K,) MADAM,MAFIA,MAGIC,MAGMA,MAGUS o,) MAIZE,MAJOR,MANGA,MANGE,MANGO ,) MANIA,MANIC,MANOR,MANSE,MANTA ·,¤) MARRY,MARSH,MASON,MATCH,MATTE Û,©) MAVEN,MAXIM,MAYBE,MAYOR,MEANT ÿ,®) MECCA,MEDAL,MEDIA,MEDIC,MELEE #-³) MELON,MERCY,MERGE,MERIT,MERRY G-¸) MESSY,METAL,METRO,MICRO,MIGHT k-½) MIMIC,MINCE,MINOR,MINUS,MIRTH -Â) MODAL,MODEL,MODEM,MOGGY,MOGUL ³-Ç) MOIST,MONEY,MOOCH,MOODY,MOOSE ×-Ì) MORON,MORPH,MOTEL,MOTIF,MOTOR û-Ñ) MOTTO,MOUND,MOUNT,MOURN,MOUSE .Ö) MOUTH,MOVIE,MUCUS,MUDDY,MULCH C.Û) MURAL,MUSIC,NACHO,NAIVE,NAKED g.à) NANNY,NAPPY,NASAL,NASTY,NAVEL .å) NERVE,NEVER,NICHE,NIECE,NIGHT ¯.ê) NINJA,NITRO,NODAL,NOISE,NOISY Ó.ï) NOMAD,NOOSE,NORTH,NOTCH,NURSE ÷.ô) NUTTY,NYLON,NYMPH,OAKEN,OASIS /ù) OCCUR,OCEAN,OCTAL,OCTET,OFFAL ?/þ) OFFER,OFTEN,OMEGA,OOZED,OPERA c/* OPIUM,OPTIC,ORATE,ORBIT,ORDER /* ORGAN,OTHER,OTTER,OUGHT,OUNCE «/* OVARY,OVERT,OWNER,OXIDE,OZONE Ï/* PAGAN,PAGER,PAINT,PALEO,PANDA ó/* PANEL,PANIC,PANSY,PANTS,PAPER 0* PARTY,PASTA,PASTE,PATCH,PATIO ;0!* PAUSE,PEACE,PEACH,PEARL,PEDAL _0&* PENAL,PENCE,PENIS,PERCH,PERIL 0+* PETAL,PETTY,PHASE,PHIAL,PHONE §00* PHONY,PHOTO,PIANO,PIECE,PILOT Ë05* PINCH,PINUP,PIQUE,PITCH,PITHY ï0:* PIXEL,PIZZA,PLACE,PLAID,PLAIN 1?* PLAIT,PLANE,PLANK,PLANT,PLATE 71D* PLAZA,PLEAD,PLEAS,PLONK,PLUMB [1I* PLUME,PLUMP,PLUSH,POACH,POINT 1N* POISE,POKER,POKES,POLAR,POLIO £1S* POLKA,POLYP,POOCH,PORCH,PORES Ç1X* POSSE,POTTY,POUCH,POUND,POWER ë1]* PRANK,PRAWN,PREEN,PRESS,PRICE 2b* PRICK,PRIDE,PRIME,PRINT,PRIOR 32g* PRISM,PRIVY,PRIZE,PROBE,PRONE W2l* PRONG,PROOF,PROUD,PROVE,PROWL {2q* PROXY,PRUDE,PULSE,PUNCH,PUPIL 2v* PUREE,PURGE,PURSE,PUSHY,PUSSY Ã2{* PUTTY,PYGMY,PYLON,QUAKE,QUALM ç2* QUARK,QUART,QUASH,QUASI,QUEER 3* QUERY,QUEST,QUEUE,QUICK,QUIET /3* QUIRE,QUIRK,QUIRT,QUITE,QUOTA S3* QUOTE,RADAR,RADIO,RAINY,RANCH w3* RANDY,RANGE,RAPID,RATIO,RAVEN 3* RAZOR,REACH,REACT,REALM,REBEL ¿3* REFER,REGAL,REHAB,REIGN,REINS ã3£* RELAX,RELAY,RELIC,REMIT,REMIX 4¨* REPLY,RESIN,RETRO,RETRY,REUSE +4­* REVEL,REVUE,RHINO,RHYME,RIDER O4²* RIDGE,RIGHT,RIGID,RIGOR,RINSE s4·* RISKY,RIVAL,ROACH,ROAST,ROBED 4¼* ROBIN,ROBOT,ROCKS,ROCKY,RODEO »4Á* ROGER,ROGUE,ROMAN,ROTOR,ROUGE ß4Æ* ROUGH,ROUND,ROUSE,ROUST,ROUTE 5Ë* ROYAL,RUBLE,RUDDY,RUGBY,RUINS '5Ð* RULER,RUNIC,RUNNY,RURAL,RUSTY K5Õ* SABLE,SAINT,SALAD,SALON,SALSA o5Ú* SALTY,SALVE,SALVO,SAMBA,SANDY 5ß* SARGE,SATAY,SATED,SATIN,SATYR ·5ä* SAUCE,SAUCY,SAUNA,SAUTE,SAVVY Û5é* SCALE,SCALP,SCALY,SCAMP,SCANT ÿ5î* SCARE,SCARF,SCARY,SCENE,SCENT #6ó* SCOFF,SCOLD,SCONE,SCOOP,SCOOT G6ø* SCOPE,SCORE,SCORN,SCOUR,SCOUT k6ý* SCOWL,SCRAM,SCRAP,SCREW,SCRUM 6+ SCUBA,SEDAN,SEMEN,SENSE,SEPIA ³6+ SERIF,SERUM,SERVE,SERVO,SETUP ×6+ SEVEN,SEVER,SEWER,SHACK,SHADE û6+ SHADY,SHAFT,SHAKE,SHAKY,SHALE 7+ SHALL,SHALT,SHAME,SHANK,SHAPE C7+ SHARD,SHARE,SHARK,SHARP,SHAVE g7 + SHAWL,SHEAF,SHEAR,SHEEN,SHEEP 7%+ SHEER,SHEET,SHELF,SHELL,SHINE ¯7*+ SHINY,SHIRE,SHIRT,SHOAL,SHOCK Ó7/+ SHOES,SHOOT,SHORE,SHORT,SHOUT ÷74+ SHOVE,SHRED,SHREW,SHRUB,SHRUG 89+ SHUNT,SIEGE,SIEVE,SIGHT,SIGMA ?8>+ SILKY,SILLY,SINCE,SINEW,SINGE c8C+ SINUS,SIREN,SIXTY,SKATE,SKEIN 8H+ SKINT,SKIRT,SKULK,SKULL,SKUNK «8M+ SLACK,SLAIN,SLANG,SLANT,SLASH Ï8R+ SLATE,SLEEK,SLEEP,SLEET,SLEPT ó8W+ SLICE,SLICK,SLIDE,SLIME,SLING 9\+ SLOOP,SLOPE,SLOSH,SLOTH,SLUMP ;9a+ SLUNG,SLUSH,SLUTS,SMACK,SMALL _9f+ SMART,SMASH,SMEAR,SMELL,SMILE 9k+ SMIRK,SMITE,SMITH,SMOCK,SMOKE §9p+ SNACK,SNAIL,SNAKE,SNARE,SNEAK Ë9u+ SNEER,SNIPE,SNOOP,SNORE,SNORT ï9z+ SNOUT,SOLAR,SOLID,SOLVE,SONAR :+ SONIC,SOOTH,SORRY,SOUND,SPACE 7:+ SPADE,SPANK,SPARE,SPARK,SPASM [:+ SPAWN,SPEAK,SPEAR,SPECK,SPEED :+ SPELL,SPEND,SPENT,SPERM,SPICE £:+ SPICY,SPIEL,SPILL,SPINE,SPIRE Ç:+ SPITE,SPOIL,SPOKE,SPOOF,SPOOK ë:+ SPOOL,SPOON,SPORE,SPORT,SPOUT ;¢+ SPRAY,SPREE,SPRIG,SPUNK,SPURN 3;§+ SPURT,SQUAD,SQUAT,SQUID,STACK W;¬+ STAFF,STAGE,STAIN,STAIR,STAKE {;±+ STALE,STALK,STALL,STAMP,STAND ;¶+ STARE,STARK,START,STASH,STATE Ã;»+ STEAK,STEAL,STEAM,STEED,STEEL ç;À+ STEEP,STEER,STENT,STERN,STICK <Å+ STIFF,STILL,STILT,STING,STINK /<Ê+ STINT,STOAT,STOCK,STOIC,STOKE S<Ï+ STOLE,STOMP,STONE,STONY,STOOD w<Ô+ STOOL,STOOP,STORE,STORK,STORM <Ù+ STORY,STOUT,STOVE,STRAP,STRAW ¿<Þ+ STRAY,STRUM,STRUT,STUCK,STUDY ã<ã+ STUFF,STUMP,STUNG,STUNT,STYLE =è+ SUAVE,SUEDE,SUGAR,SUITE,SUNNY +=í+ SUPER,SURGE,SURLY,SUSHI,SWAMP O=ò+ SWARM,SWEAR,SWEAT,SWEDE,SWEEP s=÷+ SWEET,SWELL,SWEPT,SWIFT,SWILL =ü+ SWINE,SWING,SWIPE,SWIRL,SWISH »=, SWOON,SWOOP,SWORD,SWORE,SWORN ß=, SYNTH,SYRUP,TABOO,TAFFY,TAINT >, TAKEN,TALON,TANGO,TANGY,TAPAS '>, TARDY,TASTE,TASTY,TAUNT,TEACH K>, TEASE,TEDDY,TEMPO,TEMPT,TENET o>, TENOR,TENSE,TENTH,TEPID,TERSE >, THEFT,THEIR,THEME,THERE,THESE ·>$, THETA,THICK,THIEF,THIGH,THING Û>), THINK,THIRD,THONG,THORN,THOSE ÿ>., THREE,THREW,THROB,THROE,THROW #?3, THUMB,THYME,TIARA,TIBIA,TIDAL G?8, TIGER,TIGHT,TIMID,TITAN,TITHE k?=, TITLE,TITTY,TOAST,TODAY,TONER ?B, TONGS,TONIC,TONNE,TOOLS,TOOTH ³?G, TOPAZ,TOPIC,TORCH,TORSO,TOTAL ×?L, TOUCH,TOUGH,TOWEL,TOWER,TOXIC û?Q, TOXIN,TRACE,TRACK,TRACT,TRADE @V, TRAIL,TRAIN,TRAIT,TRAMP,TRAWL C@[, TREAD,TREAT,TREND,TRESS,TRIAD g@`, TRIAL,TRIBE,TROLL,TROUT,TROVE @e, TRUCE,TRUCK,TRUNK,TRUSS,TRUST ¯@j, TRUTH,TRYST,TULIP,TUNER,TUNIC Ó@o, TURBO,TURDS,TUTOR,TWEAK,TWEED ÷@t, TWICE,TWINE,TWIST,UDDER,ULCER Ay, ULTRA,UNCAP,UNCLE,UNCUT,UNDER ?A~, UNDUE,UNIFY,UNION,UNITE,UNLIT cA, UNTIE,UNTIL,UNZIP,UPPER,UPSET A, URBAN,USAGE,USUAL,USURP,USURY «A, UTTER,VAGUE,VALET,VALID,VALUE ÏA, VALVE,VAPID,VAULT,VAUNT,VENOM óA, VENUE,VERGE,VERSE,VERVE,VICAR B, VIDEO,VIGIL,VIGOR,VILLA,VINYL ;B¡, VIOLA,VIPER,VIRAL,VIRUS,VISIT _B¦, VISOR,VISTA,VITAL,VIVID,VIXEN B«, VOCAL,VODKA,VOGUE,VOMIT,VOUCH §B°, VOWEL,VULVA,WAFER,WAGER,WAGES ËBµ, WAGON,WAIST,WAIVE,WALTZ,WASTE ïBº, WATCH,WATER,WAXEN,WEARY,WEAVE C¿, WEDGE,WEIGH,WEIRD,WELSH,WENCH 7CÄ, WHACK,WHALE,WHARF,WHEAT,WHEEL [CÉ, WHELM,WHERE,WHICH,WHIFF,WHILE CÎ, WHINE,WHIRL,WHITE,WHOLE,WHORE £CÓ, WHOSE,WIDOW,WIDTH,WIELD,WINCE ÇCØ, WINCH,WITCH,WITTY,WOMAN,WOMEN ëCÝ, WOODS,WOODY,WORLD,WORRY,WORSE Dâ, WORST,WORTH,WOULD,WOUND,WOVEN 3Dç, WRATH,WREAK,WRECK,WREST,WRING WDì, WRIST,WRITE,WRONG,WURST,XENON {Dñ, YACHT,YODEL,YOUNG,YOUTH,YUMMY Dö, ZEBRA   