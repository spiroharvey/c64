

;wordle_v12.prg ==0801==
   10 print"{clr}"
   20 poke 53280,0 :poke 53281,0
   30 a$="UCI"
   31 b$="B B"
   32 c$="JCK"
   33 p$="{red}Z" : at=0
   35 dim ga$(5)
   40 read nw:dim wd$(nw)
   45 ds=peek(66)*256+peek(65)+6
   50 printtab(15)"{yel}w o r d l e         {cyn}v1.2"
   51 print"{gry3}DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
   55 print" {pur}c=64 conversion by spiro harvey, 2022"
   56 print"         {gry1}original by josh wardle"
   57 print"    {lgrn}special thanks to dan sanderson"
   59 print"{gry3}DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
   60 gosub 2000 : rem load data
   62 print"{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}{left}                                {up}{up}{up}"
   65 printtab(15);"{down}{wht}how to play"
   70 print" {down}{gry3}guess the {yel}wordle{gry3} in 6 tries."
   80 print" {down}after each guess, the colour of the"
   90 print" tiles will change to show how close"
  100 print" your guess was to the word."
  110 print"{down}DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
  111 print" {cyn}press {rvon}return{rvof} to start; {rvon}space{rvof} for help"
  112 get k$:if k$="" then 112
  113 if k$=chr$(13) then goto 300
  114 if k$=" " then print"{up}                                       {up}":goto 120
  115 goto 112
  120 print"    {lgrn}";a$;"{wht}";a$;a$;a$;a$
  125 print"    {lgrn}BwB{wht}BeBBaBBrBByB"
  130 print"    {lgrn}";c$;"{wht}";c$;c$;c$;c$
  140 print" {gry3}the letter {lgrn}w{gry3} is in the word and in"
  145 print" the correct spot."
  150 print"    {wht}";a$;a$;"{brn}";a$;"{wht}";a$;a$
  155 print"    {wht}BpBBiB{brn}BlB{wht}BoBBtB"
  160 print"    {wht}";c$;c$;"{brn}";c$;"{wht}";c$;c$
  165 print" {gry3}the letter {brn}l{gry3} is in the word, but at"
  170 print" wrong spot."
  180 gosub 250
  200 print"{wht}    ";a$;a$;a$;"{gry1}";a$;"{wht}";a$
  205 print"    {wht}BvBBaBBgB{gry1}BuB{wht}BeB          "
  210 print"    {wht}";c$;c$;c$;"{gry1}";c$;"{wht}";c$
  215 print" {gry3}the letter {gry1}u{gry3} is not in the word."
  220 gosub 250
  230 goto 300
  250 rem ------------- screen pause ---
  255 print"        {down}{lred}press {rvon}enter{rvof} to continue...{gry3}"
  260 get k$:if k$="" then 9010
  265 if k$<>chr$(13) then 9010
  270 print"{up}                                      {up}{up}"
  275 return
  300 rem -------------- game screen ---
  305 r=int(rnd(1)*nw)
  310 pw$=wd$(r)
  315 print"{clr}"
  320 printtab(15)"{yel}w o r d l e"
  325 print"{wht}"
  330 for c=1 to 6
  335 printtab(13);a$;a$;a$;a$;a$
  340 printtab(13);b$;b$;b$;b$;b$
  345 printtab(13);c$;c$;c$;c$;c$
  350 next
  400 rem ---------------- main loop ---
  405 for at=1 to 6 : rem attempt num
  410 for i=1 to 5:ga$(i)="":next
  415 gs$=""
  420 gosub 680 : rem update line
  430 gosub 660 : rem hilight line
  440 gosub 600 : rem read guess
  450 next
  460 rem ----------------- end game ---
  470 printtab(7);"{yel}sorry, the word was {lred}"pw$"{yel}."
  480 goto 530
  500 rem --- you win!
  510 print"{home}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}";
  520 printtab(12);"{cyn}huzzah!! you win!{gry3}"
  530 print"would you like to play again? (y/n)";
  540 get k$
  550 if k$="n" then end
  560 if k$="y" then goto 300
  570 goto 540
  600 rem --------------- read guess ---
  605 c=0 : rem char count
  610 get k$ : if k$="" then 610
  620 if k$=chr$(20) and c>0 then goto 700
  630 if k$=chr$(13) and c>=4 then goto 800: return
  640 if k$>="a" and k$<="z" then goto 750
  650 goto 610
  655 return
  660 rem -------- hilight curr line ---
  665 poke 1024+(ln*40)+11,90
  670 poke 55296+(ln*40)+11,10
  675 return
  680 rem -------------- update line ---
  685 poke 1024+(ln*40)+11,32
  690 ln=(at*3)+1
  695 return
  700 rem ---------------- del char ---
  705 x=c-1
  710 gs$=left$(gs$,x)
  715 poke 1024+(ln*40)+(x*3)+14,32
  720 c=x
  725 goto 610 : rem next guess
  750 rem ------------- accept char ---
  755 if c>=5 then goto 610
  760 gs$=gs$+k$
  765 poke 1024+(ln*40)+(c*3)+14,asc(k$)-64
  770 c=c+1
  775 goto 610
  800 rem ---------------- find word ---
  801 rem find gs$ in data quickly
  802 rem r = result:-1 if found, 0 if not
  803 rem expects nw=# of words, ds=addr of first word
  804 if gs$=pw$ then goto 950
  805 sc$="DMBN" : sc=1
  808 lo=0:hi=nw
  810 i=int((hi-lo)/2)+lo
  815 gosub 850
  817 gosub 880
  818 if sc=5 then sc=1
  820 if gs$=w$ then r=-1:goto 950
  825 if gs$<w$ then 840
  830 if lo=i then r=0:goto 900
  835 lo=i:goto 810
  840 if hi=i then r=0:goto 900
  845 hi=i:goto 810
  850 rem --- set w$ to the i-th word ---
  855 ia=ds+36*int(i/5)+(i-int(i/5)*5)*6
  860 if i=int(i/5)*5 then ia=ia-6
  865 poke 66,int(ia/256):poke 65,ia-(int(ia/256)*256)
  870 read w$
  875 return
  880 rem --- spinner
  882 print"{home}";
  885 zm=-1
  890 printtab(39);"{yel}";mid$(sc$,sc,1);
  895 sc=sc+1
  897 return
  900 rem --- word not in dict
  901 print"{home}";tab(39)" "
  905 print"{home}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}";
  910 printtab(10)"word not in dictionary!"
  915 printtab(10)"press a key to continue"
  920 get k$ : if k$="" then goto 920
  925 print"{home}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}";
  930 printtab(10)"                       "
  935 printtab(10)"                       "
  940 goto 610
  950 rem --- solve
  951 print"{home}";tab(39)" "
  955 for z=1 to 5
  960 for zb=1 to 5
  965 if mid$(gs$,z,1)=mid$(pw$,zb,1) then ga$(z)="-"
  970 next zb
  975 if mid$(gs$,z,1)=mid$(pw$,z,1) then ga$(z)="+"
  980 next z
 1000 rem --- solve colours
 1005 for z=1 to 5
 1010 if ga$(z)="+" then cl=13:gosub 1100
 1020 if ga$(z)="-" then cl=9:gosub 1100
 1030 if ga$(z)=""  then cl=11:gosub 1100
 1040 next
 1050 if gs$=pw$ then goto 500
 1060 goto 655
 1100 rem ----------- hilight boxes ---
 1105 for c=1 to 3
 1110 poke 55296+((ln-1)*40)+(z*3)+9+c,cl
 1115 next
 1120 for c=1 to 3
 1125 poke 55296+(ln*40)+(z*3)+9+c,cl
 1130 next
 1135 for c=1 to 3
 1140 poke 55296+((ln+1)*40)+(z*3)+9+c,cl
 1145 next
 1150 return
 2000 rem ------------- data loader ---
 2010 printtab(12)"{down}{down}{cyn}loading data...";
 2020 c=1
 2030 for w=0 to nw
 2040 read wd$(w)
 2045 if c=1 then print"{left}{left}{left}{brn}.{cyn}..";
 2050 if c=2 then print"{left}{left}{left}{cyn}.{brn}.{cyn}.";
 2055 if c=3 then print"{left}{left}{left}{cyn}..{brn}.";
 2060 if c=4 then c=0
 2065 c=c+1
 2070 next
 2099 return
 9999 data 1510
10000 data abbey,abbot,abhor,abide,abode
10005 data abort,about,above,abyss,acorn
10010 data actor,adept,admin,adopt,adore
10015 data adorn,adult,afire,afoot,afoul
10020 data after,again,agony,agree,ahead
10025 data aided,aisle,alarm,album,alien
10030 data align,allow,aloft,aloha,amaze
10035 data amuse,angel,anvil,apple,argon
10040 data arson,audio,audit,avoid,awake
10045 data bacon,badge,bagel,baggy,baker
10050 data balls,banjo,barge,basic,basin
10055 data basis,batch,bawdy,beach,beast
10060 data beats,began,begat,belch,belly
10065 data below,beret,berry,bevvy,bigot
10070 data biker,bimbo,binge,black,blade
10075 data blame,bland,blank,blast,blaze
10080 data blood,bloom,blown,bluff,blunt
10085 data blurb,blurt,blush,board,bogus
10090 data boner,boobs,booth,bored,bossy
10095 data bound,brace,braid,brain,brake
10100 data brand,brash,brass,brave,brawl
10105 data bread,break,brick,bride,brief
10110 data brook,brute,buggy,bulge,bunch
10115 data bunny,burly,butch,buxom,bylaw
10120 data cabal,cabin,cable,cache,cadet
10125 data camel,canal,candy,canoe,canon
10130 data caper,carat,carry,carve,catch
10135 data cater,caulk,cause,cease,cedar
10140 data cello,chair,chalk,champ,chant
10145 data chaos,charm,chase,chasm,cheap
10150 data cheat,check,cheek,cheer,chick
10155 data chief,child,chime,chirp,choke
10160 data chord,chore,chunk,chute,civic
10165 data civil,clamp,clash,clasp,class
10170 data claws,clean,clear,click,cliff
10175 data climb,cloak,clock,clone,cloth
10180 data cloud,clown,clump,coach,coast
10185 data cobra,codex,colon,comic,copse
10190 data coral,couch,cough,could,count
10195 data crack,craft,cramp,crane,crank
10200 data crash,crass,crate,crave,crawl
10205 data cream,creek,creep,crime,crisp
10210 data croak,croft,cross,crowd,crown
10215 data crude,cruel,crumb,crush,crust
10220 data crypt,curio,cycle,cynic,daily
10225 data dairy,daisy,dance,dandy,datum
10230 data daunt,death,debit,debug,debut
10235 data decal,decay,decor,decoy,delay
10240 data delta,denim,dense,depot,depth
10245 data devil,diary,digit,dildo,diner
10250 data dirge,dirty,disco,ditch,ditto
10255 data dodge,dogma,donor,doubt,dough
10260 data douse,dozen,draft,drain,drama
10265 data drank,drape,drawl,drawn,dread
10270 data drift,drill,drink,drive,drone
10275 data drool,droop,drove,druid,dwarf
10280 data dwell,eager,eagle,early,earth
10285 data easel,ebony,edict,edify,eerie
10290 data eight,eject,elbow,elder,elect
10295 data elite,email,embed,ember,empty
10300 data enact,enema,enemy,enjoy,enter
10305 data entry,envoy,epoch,epoxy,equal
10310 data equip,erase,erect,error,erupt
10315 data essay,evade,event,evict,evoke
10320 data exact,exile,exist,fable,facet
10325 data faint,fairy,false,fancy,fanny
10330 data fatal,feast,feign,felon,fence
10335 data feral,ferry,fetch,fever,fewer
10340 data fight,filth,final,flail,flair
10345 data flake,flame,flank,flare,flash
10350 data flask,fleet,flesh,flint,flirt
10355 data float,flock,flood,floor,flour
10360 data fluff,fluid,fluke,flush,flute
10365 data focus,folio,foray,force,forge
10370 data forgo,forth,found,frail,frame
10375 data fraud,freak,fresh,frill,frisk
10380 data frock,frost,froth,frown,froze
10385 data fruit,fudge,fungi,fussy,futon
10390 data gable,gamma,gamut,gaudy,gauge
10395 data gaunt,gavel,geese,genie,genre
10400 data ghost,ghoul,giant,gizmo,glade
10405 data gland,glass,glaze,gleam,glean
10410 data glint,gloat,globe,gloom,gloss
10415 data glove,glyph,gnash,gnome,goose
10420 data graft,grail,grain,grand,graph
10425 data grasp,grass,grate,grave,gravy
10430 data graze,great,greed,green,greet
10435 data grief,grill,grime,grind,groan
10440 data groom,grunt,guard,guess,guest
10445 data guide,guild,guile,guilt,guise
10450 data gypsy,habit,haiku,hairy,happy
10455 data hardy,harem,harpy,harsh,haste
10460 data hasty,haunt,haven,havoc,heady
10465 data heard,heart,heave,heavy,hedge
10470 data hefty,heist,hiker,hinge,hippo
10475 data hippy,hitch,hoard,hobby,hoist
10480 data honey,horde,horns,horny,horse
10485 data hotel,hound,house,hovel,hover
10490 data howdy,human,humid,hunch,hurry
10495 data hydro,hyena,ideal,idiom,idiot
10500 data igloo,image,imbue,impel,imply
10505 data inane,incur,index,indie,inept
10510 data inert,infer,ingot,inked,inlay
10515 data inlet,inner,inset,irate,irony
10520 data issue,ivory,jaded,jaunt,jeans
10525 data jelly,jetty,jewel,jiffy,joint
10530 data joist,joker,jolly,joule,joust
10535 data jowls,judge,juice,jumbo,juror
10540 data kanji,karat,karma,kayak,kebab
10545 data khaki,kinky,kiosk,klutz,knack
10550 data knave,knead,kneed,kneel,knell
10555 data knife,knock,knoll,known,koala
10560 data krone,kudos,label,labia,laces
10565 data laden,ladle,lager,laird,lamer
10570 data lance,lanky,lapel,lapse,large
10575 data larva,laser,latch,later,latex
10580 data lathe,latte,laugh,layer,leaky
10585 data learn,lease,leash,least,leave
10590 data ledge,leech,leery,legal,legit
10595 data lemon,lemur,leper,levee,level
10600 data lever,libel,liege,light,lilac
10605 data limbo,limit,linen,lingo,lithe
10610 data liven,liver,livid,llama,loamy
10615 data loath,lobby,local,locus,lodge
10620 data logic,lolly,loner,loose,lorry
10625 data loser,lotus,louse,lover,lower
10630 data lowly,loyal,lucid,lucky,lumen
10635 data lunar,lunch,lunge,lupus,lurch
10640 data lurid,lynch,lyric,macho,macro
10645 data madam,mafia,magic,magma,magus
10650 data maize,major,manga,mange,mango
10655 data mania,manic,manor,manse,manta
10660 data marry,marsh,mason,match,matte
10665 data maven,maxim,maybe,mayor,meant
10670 data mecca,medal,media,medic,melee
10675 data melon,mercy,merge,merit,merry
10680 data messy,metal,metro,micro,might
10685 data mimic,mince,minor,minus,mirth
10690 data modal,model,modem,moggy,mogul
10695 data moist,money,mooch,moody,moose
10700 data moron,morph,motel,motif,motor
10705 data motto,mound,mount,mourn,mouse
10710 data mouth,movie,mucus,muddy,mulch
10715 data mural,music,nacho,naive,naked
10720 data nanny,nappy,nasal,nasty,navel
10725 data nerve,never,niche,niece,night
10730 data ninja,nitro,nodal,noise,noisy
10735 data nomad,noose,north,notch,nurse
10740 data nutty,nylon,nymph,oaken,oasis
10745 data occur,ocean,octal,octet,offal
10750 data offer,often,omega,oozed,opera
10755 data opium,optic,orate,orbit,order
10760 data organ,other,otter,ought,ounce
10765 data ovary,overt,owner,oxide,ozone
10770 data pagan,pager,paint,paleo,panda
10775 data panel,panic,pansy,pants,paper
10780 data party,pasta,paste,patch,patio
10785 data pause,peace,peach,pearl,pedal
10790 data penal,pence,penis,perch,peril
10795 data petal,petty,phase,phial,phone
10800 data phony,photo,piano,piece,pilot
10805 data pinch,pinup,pique,pitch,pithy
10810 data pixel,pizza,place,plaid,plain
10815 data plait,plane,plank,plant,plate
10820 data plaza,plead,pleas,plonk,plumb
10825 data plume,plump,plush,poach,point
10830 data poise,poker,pokes,polar,polio
10835 data polka,polyp,pooch,porch,pores
10840 data posse,potty,pouch,pound,power
10845 data prank,prawn,preen,press,price
10850 data prick,pride,prime,print,prior
10855 data prism,privy,prize,probe,prone
10860 data prong,proof,proud,prove,prowl
10865 data proxy,prude,pulse,punch,pupil
10870 data puree,purge,purse,pushy,pussy
10875 data putty,pygmy,pylon,quake,qualm
10880 data quark,quart,quash,quasi,queer
10885 data query,quest,queue,quick,quiet
10890 data quire,quirk,quirt,quite,quota
10895 data quote,radar,radio,rainy,ranch
10900 data randy,range,rapid,ratio,raven
10905 data razor,reach,react,realm,rebel
10910 data refer,regal,rehab,reign,reins
10915 data relax,relay,relic,remit,remix
10920 data reply,resin,retro,retry,reuse
10925 data revel,revue,rhino,rhyme,rider
10930 data ridge,right,rigid,rigor,rinse
10935 data risky,rival,roach,roast,robed
10940 data robin,robot,rocks,rocky,rodeo
10945 data roger,rogue,roman,rotor,rouge
10950 data rough,round,rouse,roust,route
10955 data royal,ruble,ruddy,rugby,ruins
10960 data ruler,runic,runny,rural,rusty
10965 data sable,saint,salad,salon,salsa
10970 data salty,salve,salvo,samba,sandy
10975 data sarge,satay,sated,satin,satyr
10980 data sauce,saucy,sauna,saute,savvy
10985 data scale,scalp,scaly,scamp,scant
10990 data scare,scarf,scary,scene,scent
10995 data scoff,scold,scone,scoop,scoot
11000 data scope,score,scorn,scour,scout
11005 data scowl,scram,scrap,screw,scrum
11010 data scuba,sedan,semen,sense,sepia
11015 data serif,serum,serve,servo,setup
11020 data seven,sever,sewer,shack,shade
11025 data shady,shaft,shake,shaky,shale
11030 data shall,shalt,shame,shank,shape
11035 data shard,share,shark,sharp,shave
11040 data shawl,sheaf,shear,sheen,sheep
11045 data sheer,sheet,shelf,shell,shine
11050 data shiny,shire,shirt,shoal,shock
11055 data shoes,shoot,shore,short,shout
11060 data shove,shred,shrew,shrub,shrug
11065 data shunt,siege,sieve,sight,sigma
11070 data silky,silly,since,sinew,singe
11075 data sinus,siren,sixty,skate,skein
11080 data skint,skirt,skulk,skull,skunk
11085 data slack,slain,slang,slant,slash
11090 data slate,sleek,sleep,sleet,slept
11095 data slice,slick,slide,slime,sling
11100 data sloop,slope,slosh,sloth,slump
11105 data slung,slush,sluts,smack,small
11110 data smart,smash,smear,smell,smile
11115 data smirk,smite,smith,smock,smoke
11120 data snack,snail,snake,snare,sneak
11125 data sneer,snipe,snoop,snore,snort
11130 data snout,solar,solid,solve,sonar
11135 data sonic,sooth,sorry,sound,space
11140 data spade,spank,spare,spark,spasm
11145 data spawn,speak,spear,speck,speed
11150 data spell,spend,spent,sperm,spice
11155 data spicy,spiel,spill,spine,spire
11160 data spite,spoil,spoke,spoof,spook
11165 data spool,spoon,spore,sport,spout
11170 data spray,spree,sprig,spunk,spurn
11175 data spurt,squad,squat,squid,stack
11180 data staff,stage,stain,stair,stake
11185 data stale,stalk,stall,stamp,stand
11190 data stare,stark,start,stash,state
11195 data steak,steal,steam,steed,steel
11200 data steep,steer,stent,stern,stick
11205 data stiff,still,stilt,sting,stink
11210 data stint,stoat,stock,stoic,stoke
11215 data stole,stomp,stone,stony,stood
11220 data stool,stoop,store,stork,storm
11225 data story,stout,stove,strap,straw
11230 data stray,strum,strut,stuck,study
11235 data stuff,stump,stung,stunt,style
11240 data suave,suede,sugar,suite,sunny
11245 data super,surge,surly,sushi,swamp
11250 data swarm,swear,sweat,swede,sweep
11255 data sweet,swell,swept,swift,swill
11260 data swine,swing,swipe,swirl,swish
11265 data swoon,swoop,sword,swore,sworn
11270 data synth,syrup,taboo,taffy,taint
11275 data taken,talon,tango,tangy,tapas
11280 data tardy,taste,tasty,taunt,teach
11285 data tease,teddy,tempo,tempt,tenet
11290 data tenor,tense,tenth,tepid,terse
11295 data theft,their,theme,there,these
11300 data theta,thick,thief,thigh,thing
11305 data think,third,thong,thorn,those
11310 data three,threw,throb,throe,throw
11315 data thumb,thyme,tiara,tibia,tidal
11320 data tiger,tight,timid,titan,tithe
11325 data title,titty,toast,today,toner
11330 data tongs,tonic,tonne,tools,tooth
11335 data topaz,topic,torch,torso,total
11340 data touch,tough,towel,tower,toxic
11345 data toxin,trace,track,tract,trade
11350 data trail,train,trait,tramp,trawl
11355 data tread,treat,trend,tress,triad
11360 data trial,tribe,troll,trout,trove
11365 data truce,truck,trunk,truss,trust
11370 data truth,tryst,tulip,tuner,tunic
11375 data turbo,turds,tutor,tweak,tweed
11380 data twice,twine,twist,udder,ulcer
11385 data ultra,uncap,uncle,uncut,under
11390 data undue,unify,union,unite,unlit
11395 data untie,until,unzip,upper,upset
11400 data urban,usage,usual,usurp,usury
11405 data utter,vague,valet,valid,value
11410 data valve,vapid,vault,vaunt,venom
11415 data venue,verge,verse,verve,vicar
11420 data video,vigil,vigor,villa,vinyl
11425 data viola,viper,viral,virus,visit
11430 data visor,vista,vital,vivid,vixen
11435 data vocal,vodka,vogue,vomit,vouch
11440 data vowel,vulva,wafer,wager,wages
11445 data wagon,waist,waive,waltz,waste
11450 data watch,water,waxen,weary,weave
11455 data wedge,weigh,weird,welsh,wench
11460 data whack,whale,wharf,wheat,wheel
11465 data whelm,where,which,whiff,while
11470 data whine,whirl,white,whole,whore
11475 data whose,widow,width,wield,wince
11480 data winch,witch,witty,woman,women
11485 data woods,woody,world,worry,worse
11490 data worst,worth,would,wound,woven
11495 data wrath,wreak,wreck,wrest,wring
11500 data wrist,write,wrong,wurst,xenon
11505 data yacht,yodel,young,youth,yummy
11510 data zebra

