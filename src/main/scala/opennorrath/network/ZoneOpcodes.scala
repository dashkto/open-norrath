package opennorrath.network

/** Application opcodes for the EQ Mac zone server protocol.
  * Values from EQMacDocker/Server/utils/patches/patch_Mac.conf.
  * These are big-endian wire values (matching how OldPacket reads opcodes).
  */
object ZoneOpcodes:

  // --- Zone Entry Handshake ---
  val ZoneEntry: Short          = 0x2840.toShort  // Client → Zone: request zone entry
  val ZoneEntryResend: Short    = 0x4141.toShort  // Client → Zone: resend zone entry
  val ZoneInAvatarSet: Short    = 0x6f40.toShort  // Client → Zone: avatar set on zone-in
  val PlayerProfile: Short      = 0x3640.toShort  // Zone → Client: full character profile (8460 bytes)
  val NewZone: Short            = 0x5b40.toShort  // Zone → Client: zone metadata
  val ReqNewZone: Short         = 0x5d40.toShort  // Client → Zone: request zone data
  val ReqClientSpawn: Short     = 0x0a40.toShort  // Client → Zone: ready for spawns
  val TimeOfDay: Short          = 0xf240.toShort  // Zone → Client: game time
  val SetServerFilter: Short    = 0xff41.toShort  // Client → Zone: message filter
  val DataRate: Short           = 0xe841.toShort  // Client → Zone: data rate

  // --- Spawns ---
  val ZoneSpawns: Short         = 0x5f41.toShort  // Zone → Client: all spawns in zone
  val NewSpawn: Short           = 0x6b42.toShort  // Zone → Client: new entity spawned
  val DeleteSpawn: Short        = 0x2940.toShort  // Zone → Client: entity removed
  val Death: Short              = 0x4a40.toShort  // Zone → Client: entity death

  // --- Movement / Client Control ---
  val ClientUpdate: Short       = 0xf340.toShort  // Client → Zone: player position
  val MobUpdate: Short          = 0x9f40.toShort  // Zone → Client: entity position update
  val FreezeClientControl: Short   = 0x6a40.toShort  // Zone → Client: freeze player movement (stun/root)
  val UnfreezeClientControl: Short = 0x6b40.toShort  // Zone → Client: unfreeze player movement
  val ReturnClientControl: Short   = 0xb040.toShort  // Zone → Client: return control to client

  // --- Combat ---
  val AutoAttack: Short         = 0x5141.toShort  // Client → Zone: toggle melee
  val AutoAttack2: Short        = 0x6141.toShort  // Client → Zone: secondary attack
  val Damage: Short             = 0x5840.toShort  // Zone → Client: damage message
  val Consider: Short           = 0x3741.toShort  // Bidirectional: consider
  val ConsiderCorpse: Short     = 0x3442.toShort  // Bidirectional: consider corpse
  val EnvDamage: Short          = 0x1e40.toShort  // Zone → Client: environmental damage
  val Taunt: Short              = 0x3b41.toShort  // Client → Zone: taunt
  val Stun: Short               = 0x5b41.toShort  // Zone → Client: stun effect
  val CombatAbility: Short      = 0x6041.toShort  // Client → Zone: combat ability (kick, bash, etc.)
  val BerserkStart: Short       = 0xa941.toShort  // Zone → Client: berserk mode start
  val BerserkEnd: Short         = 0xaa41.toShort  // Zone → Client: berserk mode end
  val Discipline: Short         = 0xe641.toShort  // Client → Zone: activate discipline
  val DisciplineChange: Short   = 0xf241.toShort  // Zone → Client: discipline state change
  val Shielding: Short          = 0x4b42.toShort  // Bidirectional: /shield
  val Projectile: Short         = 0x4540.toShort  // Zone → Client: projectile visual (arrow, bolt)

  // --- Appearance / Animation ---
  val SpawnAppearance: Short    = 0xf540.toShort  // Bidirectional: appearance changes
  val WearChange: Short         = 0x9240.toShort  // Zone → Client: equipment visual change
  val Animation: Short          = 0xa140.toShort  // Zone → Client: mob animation
  val Action: Short             = 0x4640.toShort  // Zone → Client: spell/combat action
  val Illusion: Short           = 0x9140.toShort  // Zone → Client: illusion (race change)
  val FaceChange: Short         = 0x2542.toShort  // Bidirectional: face change
  val Medding: Short            = 0x5841.toShort  // Zone → Client: meditate appearance
  val Surname: Short            = 0xc441.toShort  // Bidirectional: surname
  val SetTitle: Short           = 0xd440.toShort  // Bidirectional: title

  // --- Chat / Messages ---
  val ChannelMessage: Short     = 0x0741.toShort  // Bidirectional: chat message
  val SpecialMesg: Short        = 0x8041.toShort  // Zone → Client: special message (also OldSpecialMesg)
  val FormattedMessage: Short   = 0x3642.toShort  // Zone → Client: formatted message
  val Emote: Short              = 0x1540.toShort  // Bidirectional: emote text (also EmoteText)
  val MultiLineMsg: Short       = 0x1440.toShort  // Zone → Client: multi-line message
  val YellForHelp: Short        = 0xda41.toShort  // Client → Zone: /yell

  // --- HP / Mana / Stats ---
  val HPUpdate: Short           = 0xb240.toShort  // Zone → Client: HP update (also MobHealth)
  val ManaChange: Short         = 0x7f41.toShort  // Zone → Client: mana change
  val ManaUpdate: Short         = 0x1942.toShort  // Zone → Client: mana update
  val Stamina: Short            = 0x5741.toShort  // Zone → Client: hunger/thirst/endurance
  val ExpUpdate: Short          = 0x9941.toShort  // Zone → Client: experience change
  val LevelUpdate: Short        = 0x9841.toShort  // Zone → Client: level up
  val SkillUpdate: Short        = 0x8941.toShort  // Zone → Client: skill change
  val SkillUpdate2: Short       = 0xbe41.toShort  // Zone → Client: skill change (alt)
  val ResetSkill: Short         = 0x3d42.toShort  // Zone → Client: reset skill

  // --- Targeting ---
  val TargetMouse: Short        = 0x6241.toShort  // Client → Zone: target by mouse click
  val TargetCommand: Short      = 0xfe41.toShort  // Client → Zone: target by command
  val Assist: Short             = 0x0042.toShort  // Client → Zone: /assist

  // --- Spells ---
  val CastSpell: Short          = 0x7e41.toShort  // Client → Zone: cast spell
  val BeginCast: Short          = 0xa940.toShort  // Zone → Client: begin cast animation
  val InterruptCast: Short      = 0x3542.toShort  // Zone → Client: interrupt cast
  val MemorizeSpell: Short      = 0x8241.toShort  // Client → Zone: mem/unmem spell
  val SwapSpell: Short          = 0xce41.toShort  // Client → Zone: swap spell gem slots
  val Buff: Short               = 0x3241.toShort  // Zone → Client: buff added/removed
  val DeleteSpell: Short        = 0x4a42.toShort  // Zone → Client: spell deleted from book
  val Charm: Short              = 0x4442.toShort  // Zone → Client: charm effect
  val Translocate: Short        = 0x0642.toShort  // Bidirectional: teleport confirmation
  val Sacrifice: Short          = 0xea41.toShort  // Client → Zone: sacrifice spell
  val TGB: Short                = 0x2042.toShort  // Client → Zone: target group buff toggle

  // --- Items / Inventory ---
  val CharInventory: Short      = 0xf641.toShort  // Zone → Client: inventory data
  val ItemPacket: Short         = 0x6441.toShort  // Zone → Client: item data
  val ItemLinkResponse: Short   = 0x6442.toShort  // Zone → Client: item link response
  val MerchantItemPacket: Short = 0x3140.toShort  // Zone → Client: merchant item data
  val TradeItemPacket: Short    = 0xdf40.toShort  // Zone → Client: trade item data
  val LootItemPacket: Short     = 0x5240.toShort  // Zone → Client: loot item data
  val ObjectItemPacket: Short   = 0xfb40.toShort  // Zone → Client: object item data (tradeskill container)
  val SummonedItem: Short       = 0x7841.toShort  // Zone → Client: summoned item
  val ContainerPacket: Short    = 0x6641.toShort  // Zone → Client: container item
  val BookPacket: Short         = 0x6541.toShort  // Zone → Client: book item
  val MoveItem: Short           = 0x2c41.toShort  // Client → Zone: move item
  val DeleteCharge: Short       = 0x4741.toShort  // Zone → Client: item charge consumed
  val MoneyUpdate: Short        = 0x0840.toShort  // Zone → Client: money change
  val MoneyOnCorpse: Short      = 0x5040.toShort  // Zone → Client: corpse money
  val MoveCoin: Short           = 0x2d41.toShort  // Client → Zone: move coin between slots
  val Split: Short              = 0x3141.toShort  // Client → Zone: split coins with group
  val Key: Short                = 0x5d42.toShort  // Zone → Client: key ring update

  // --- Loot / Corpse ---
  val LootRequest: Short        = 0x4e40.toShort  // Client → Zone: open corpse
  val LootItem: Short           = 0xa040.toShort  // Client → Zone: loot item
  val LootComplete: Short       = 0x4541.toShort  // Zone → Client: loot done
  val EndLootRequest: Short     = 0x4f40.toShort  // Client → Zone: close corpse
  val CorpseDrag: Short         = 0x1441.toShort  // Client → Zone: drag corpse
  val CorpsePosition: Short     = 0x2140.toShort  // Zone → Client: corpse position update
  val Consent: Short            = 0xb740.toShort  // Client → Zone: /consent
  val ConsentResponse: Short    = 0xd540.toShort  // Zone → Client: consent response

  // --- Zone Objects / Tradeskill ---
  val SpawnDoor: Short          = 0xf741.toShort  // Zone → Client: door spawns
  val GroundSpawn: Short        = 0x2c40.toShort  // Zone → Client: ground items
  val SendZonepoints: Short     = 0xb440.toShort  // Zone → Client: zone lines
  val ClickDoor: Short          = 0x8d40.toShort  // Client → Zone: click door
  val MoveDoor: Short           = 0x8e40.toShort  // Zone → Client: door animation
  val UpdateDoor: Short         = 0x9840.toShort  // Zone → Client: door state update
  val DespawnDoor: Short        = 0x9b40.toShort  // Zone → Client: remove door
  val ClickObject: Short        = 0x2b40.toShort  // Client → Zone: click world object
  val ClickObjectAction: Short  = 0xd740.toShort  // Zone → Client: object action response
  val ClearObject: Short        = 0x0542.toShort  // Zone → Client: clear object
  val TradeSkillCombine: Short  = 0x0541.toShort  // Client → Zone: tradeskill combine

  // --- Environment / Transport ---
  val Weather: Short            = 0x3641.toShort  // Zone → Client: weather change
  val Weather2: Short           = 0x8a41.toShort  // Zone → Client: weather alt
  val BoardBoat: Short          = 0xbb41.toShort  // Client → Zone: board boat
  val LeaveBoat: Short          = 0xbc41.toShort  // Client → Zone: leave boat
  val ControlBoat: Short        = 0x2641.toShort  // Client → Zone: control boat

  // --- Zone Movement / Logout ---
  val ZoneChange: Short         = 0xa340.toShort  // Bidirectional: zone change
  val RequestClientZoneChange: Short = 0x4d41.toShort // Zone → Client: request zone
  val SaveOnZoneReq: Short      = 0x5541.toShort  // Client → Zone: save before zone
  val Camp: Short               = 0x0742.toShort  // Client → Zone: camp
  val Logout: Short             = 0x5041.toShort  // Bidirectional: logout
  val LogoutReply: Short        = 0x5941.toShort  // Zone → Client: logout confirmed
  val LogoutWorld: Short        = 0x3441.toShort  // Client → Zone: logout to world

  // --- Social ---
  val WhoAllRequest: Short      = 0xf440.toShort  // Client → Zone: /who
  val WhoAllResponse: Short     = 0x0b20.toShort  // Zone → Client: /who results
  val FriendsWho: Short         = 0xc541.toShort  // Client → Zone: /friends who
  val RandomReq: Short          = 0xe741.toShort  // Client → Zone: /random
  val RandomReply: Short        = 0x1640.toShort  // Zone → Client: random result
  val LFGCommand: Short         = 0xf041.toShort  // Client → Zone: /lfg
  val Bug: Short                = 0xb340.toShort  // Client → Zone: /bug
  val Feedback: Short           = 0x3c41.toShort  // Client → Zone: /feedback
  val Report: Short             = 0x5a42.toShort  // Client → Zone: /report

  // --- Group / Raid ---
  val GroupDisband: Short       = 0x4440.toShort  // Bidirectional: disband group
  val GroupInvite: Short        = 0x3e20.toShort  // Client → Zone: invite
  val GroupInvite2: Short       = 0x4040.toShort  // Client → Zone: invite (alt, sometimes sent instead)
  val GroupFollow: Short        = 0x3d20.toShort  // Client → Zone: accept invite
  val GroupCancelInvite: Short  = 0x4140.toShort  // Client → Zone: decline invite
  val GroupUpdate: Short        = 0x2620.toShort  // Zone → Client: group update
  val RaidInvite: Short         = 0x5f42.toShort  // Bidirectional: raid invite
  val RaidUpdate: Short         = 0x6042.toShort  // Zone → Client: raid update

  // --- Guild ---
  val GuildMOTD: Short          = 0x0442.toShort  // Zone → Client: guild motd
  val SetGuildMOTD: Short       = 0x0342.toShort  // Client → Zone: set guild motd
  val GuildInvite: Short        = 0x1741.toShort  // Bidirectional
  val GuildInviteAccept: Short  = 0x1841.toShort  // Client → Zone
  val GuildRemove: Short        = 0x1941.toShort  // Client → Zone
  val GuildDelete: Short        = 0x1a41.toShort  // Client → Zone
  val GuildLeader: Short        = 0x9541.toShort  // Client → Zone: set guild leader
  val GuildPeace: Short         = 0x9141.toShort  // Client → Zone: guild peace
  val GuildWar: Short           = 0x6f41.toShort  // Client → Zone: guild war
  val GuildAdded: Short         = 0x7b41.toShort  // Zone → Client: added to guild
  val GetGuildsList: Short      = 0x2841.toShort  // Client → Zone: request guild list

  // --- Trading ---
  val TradeRequest: Short       = 0xd140.toShort  // Bidirectional
  val TradeRequestAck: Short    = 0xe640.toShort  // Zone → Client: trade request acknowledged
  val TradeAcceptClick: Short   = 0xda40.toShort  // Client → Zone
  val TradeCoins: Short         = 0xe440.toShort  // Client → Zone: set trade coins
  val TradeMoneyUpdate: Short   = 0x3d41.toShort  // Zone → Client: trade money update
  val FinishTrade: Short        = 0xdc40.toShort  // Bidirectional
  val CancelTrade: Short        = 0xdb40.toShort  // Bidirectional
  val TradeReset: Short         = 0x1040.toShort  // Zone → Client: reset trade window
  val TradeRefused: Short       = 0xd640.toShort  // Zone → Client: trade refused

  // --- Merchant / Bazaar ---
  val ShopRequest: Short        = 0x0b40.toShort  // Client → Zone
  val ShopPlayerBuy: Short      = 0x3540.toShort  // Client → Zone
  val ShopPlayerSell: Short     = 0x2740.toShort  // Client → Zone
  val ShopEnd: Short            = 0x3740.toShort  // Bidirectional
  val ShopEndConfirm: Short     = 0x4641.toShort  // Zone → Client: shop end confirmed
  val ShopDelItem: Short        = 0x3840.toShort  // Zone → Client: remove item from shop
  val ShopInventoryPacket: Short = 0x0c40.toShort  // Zone → Client: merchant inventory
  val BecomeTrader: Short       = 0x1842.toShort  // Client → Zone: become bazaar trader
  val TraderShop: Short         = 0x1642.toShort  // Bidirectional: browse trader
  val Trader: Short             = 0x1242.toShort  // Bidirectional: trader actions
  val TraderBuy: Short          = 0x2442.toShort  // Client → Zone: buy from trader
  val BazaarSearch: Short       = 0x1142.toShort  // Client → Zone: bazaar search

  // --- Duel ---
  val RequestDuel: Short        = 0xcf40.toShort  // Client → Zone: /duel
  val DuelResponse: Short       = 0xd040.toShort  // Client → Zone: accept/decline duel
  val DuelResponse2: Short      = 0x5d41.toShort  // Zone → Client: duel result

  // --- Resurrect ---
  val RezzRequest: Short        = 0x2a41.toShort  // Zone → Client: resurrection offered
  val RezzAnswer: Short         = 0x9b41.toShort  // Client → Zone: accept/decline rez
  val RezzComplete: Short       = 0xec41.toShort  // Zone → Client: resurrection complete

  // --- Class Skills ---
  val FeignDeath: Short         = 0xac40.toShort  // Client → Zone: feign death
  val Mend: Short               = 0x9d41.toShort  // Client → Zone: monk mend
  val Track: Short              = 0x8441.toShort  // Client → Zone: ranger tracking
  val Fishing: Short            = 0x8f41.toShort  // Client → Zone: fishing
  val InstillDoubt: Short       = 0x9c41.toShort  // Client → Zone: instill doubt
  val ApplyPoison: Short        = 0xba41.toShort  // Client → Zone: apply poison
  val Begging: Short            = 0x2541.toShort  // Client → Zone: begging
  val SafeFallSuccess: Short    = 0xab41.toShort  // Zone → Client: safe fall succeeded
  val Hide: Short               = 0x8641.toShort  // Client → Zone: hide
  val Sneak: Short              = 0x8541.toShort  // Client → Zone: sneak
  val CancelSneakHide: Short    = 0x5a41.toShort  // Client → Zone: cancel sneak/hide
  val SenseTraps: Short         = 0x8841.toShort  // Client → Zone: sense traps
  val DisarmTraps: Short        = 0xf341.toShort  // Client → Zone: disarm traps
  val PickPocket: Short         = 0xad40.toShort  // Client → Zone: pick pocket
  val Disarm: Short             = 0xaa40.toShort  // Client → Zone: disarm

  // --- AA ---
  val AAAction: Short           = 0x1442.toShort  // Client → Zone: AA purchase/activate
  val RespondAA: Short          = 0x1542.toShort  // Zone → Client: AA data
  val AAExpUpdate: Short        = 0x2342.toShort  // Zone → Client: AA exp update

  // --- Pet ---
  val PetCommands: Short        = 0x4542.toShort  // Client → Zone

  // --- Inspect ---
  val InspectRequest: Short     = 0xb540.toShort  // Client → Zone
  val InspectAnswer: Short      = 0xb640.toShort  // Zone → Client

  // --- GM / Training ---
  val GMServers: Short          = 0xa840.toShort  // Client → Zone: /servers
  val GMZoneRequest: Short      = 0x4f41.toShort  // Client → Zone: /zone
  val GMZoneRequest2: Short     = 0x0842.toShort  // Client → Zone: /zone (alt)
  val GMHideMe: Short           = 0xd441.toShort  // Client → Zone: /hideme
  val GMGoto: Short             = 0x6e40.toShort  // Client → Zone: /goto
  val GMSummon: Short           = 0xc540.toShort  // Client → Zone: /summon
  val GMKick: Short             = 0x6d40.toShort  // Client → Zone: /kick
  val GMKill: Short             = 0x6c40.toShort  // Client → Zone: /kill
  val GMToggle: Short           = 0xde41.toShort  // Client → Zone: /toggle
  val GMFind: Short             = 0x6940.toShort  // Client → Zone: /find
  val GMDelCorpse: Short        = 0xe941.toShort  // Client → Zone: /delcorpse
  val GMSearchCorpse: Short     = 0xa741.toShort  // Client → Zone: /searchcorpse
  val GMEmoteZone: Short        = 0xe341.toShort  // Client → Zone: /emotezone
  val GMNameChange: Short       = 0xcb40.toShort  // Client → Zone: name change
  val GMLastName: Short         = 0x6e41.toShort  // Client → Zone: last name change
  val GMBecomeNPC: Short        = 0x8c41.toShort  // Client → Zone: become NPC
  val GMTraining: Short         = 0x9c40.toShort  // Zone → Client: open trainer window
  val GMEndTraining: Short      = 0x9d40.toShort  // Client → Zone: close trainer window
  val GMTrainSkill: Short       = 0x4041.toShort  // Client → Zone: train skill
  val GMEndTrainingResponse: Short = 0x4341.toShort // Zone → Client: training complete

  // --- Petition ---
  val Petition: Short           = 0x0e40.toShort  // Client → Zone: /petition
  val PetitionCheckout: Short   = 0x8e41.toShort  // GM: checkout petition
  val PetitionCheckIn: Short    = 0x9e41.toShort  // GM: check in petition (conf says 0x09e41, likely typo)
  val PetitionDelete: Short     = 0xa041.toShort  // GM: delete petition
  val DeletePetition: Short     = 0x6422.toShort  // GM: delete petition (alt)
  val PetitionRefresh: Short    = 0x1140.toShort  // Zone → Client: refresh petition list
  val PetitionViewPetition: Short = 0x6142.toShort // GM: view petition

  // --- SoulMark ---
  val SoulMarkAdd: Short        = 0xD241.toShort  // Zone → Client: add soul mark
  val SoulMarkList: Short       = 0xD141.toShort  // Zone → Client: soul mark list
  val SoulMarkUpdate: Short     = 0xD041.toShort  // Zone → Client: soul mark update

  // --- Sound ---
  val PlaySound: Short          = 0x4840.toShort  // Zone → Client

  // --- Misc ---
  val Save: Short               = 0x2e40.toShort  // Client → Zone
  val Jump: Short               = 0x2040.toShort  // Client → Zone
  val SetRunMode: Short         = 0x1f40.toShort  // Client → Zone
  val Consume: Short            = 0x5641.toShort  // Client → Zone: eat/drink
  val ReadBook: Short           = 0xce40.toShort  // Client → Zone
  val Forage: Short             = 0x9440.toShort  // Client → Zone
  val SenseHeading: Short       = 0x8741.toShort  // Client → Zone
  val Bind_Wound: Short         = 0x9340.toShort  // Client → Zone
  val SafePoint: Short          = 0x2440.toShort  // Zone → Client
  val SendExpZonein: Short      = 0xd840.toShort  // Zone → Client
  val Reward: Short             = 0x8040.toShort  // Zone → Client: reward
  val ExploreUnknown: Short     = 0x0292.toShort  // Unknown explorer

  def name(op: Short): String = op match
    // Zone Entry
    case ZoneEntry             => "ZoneEntry"
    case ZoneEntryResend       => "ZoneEntryResend"
    case ZoneInAvatarSet       => "ZoneInAvatarSet"
    case PlayerProfile         => "PlayerProfile"
    case NewZone               => "NewZone"
    case ReqNewZone            => "ReqNewZone"
    case ReqClientSpawn        => "ReqClientSpawn"
    case TimeOfDay             => "TimeOfDay"
    case SetServerFilter       => "SetServerFilter"
    case DataRate              => "DataRate"
    // Spawns
    case ZoneSpawns            => "ZoneSpawns"
    case NewSpawn              => "NewSpawn"
    case DeleteSpawn           => "DeleteSpawn"
    case Death                 => "Death"
    // Movement
    case ClientUpdate          => "ClientUpdate"
    case MobUpdate             => "MobUpdate"
    case FreezeClientControl   => "FreezeClientControl"
    case UnfreezeClientControl => "UnfreezeClientControl"
    case ReturnClientControl   => "ReturnClientControl"
    // Combat
    case AutoAttack            => "AutoAttack"
    case AutoAttack2           => "AutoAttack2"
    case Damage                => "Damage"
    case Consider              => "Consider"
    case ConsiderCorpse        => "ConsiderCorpse"
    case EnvDamage             => "EnvDamage"
    case Taunt                 => "Taunt"
    case Stun                  => "Stun"
    case CombatAbility         => "CombatAbility"
    case BerserkStart          => "BerserkStart"
    case BerserkEnd            => "BerserkEnd"
    case Discipline            => "Discipline"
    case DisciplineChange      => "DisciplineChange"
    case Shielding             => "Shielding"
    case Projectile            => "Projectile"
    // Appearance / Animation
    case SpawnAppearance       => "SpawnAppearance"
    case WearChange            => "WearChange"
    case Animation             => "Animation"
    case Action                => "Action"
    case Illusion              => "Illusion"
    case FaceChange            => "FaceChange"
    case Medding               => "Medding"
    case Surname               => "Surname"
    case SetTitle              => "SetTitle"
    // Chat / Messages
    case ChannelMessage        => "ChannelMessage"
    case SpecialMesg           => "SpecialMesg"
    case FormattedMessage      => "FormattedMessage"
    case Emote                 => "Emote"
    case MultiLineMsg          => "MultiLineMsg"
    case YellForHelp           => "YellForHelp"
    // HP / Mana / Stats
    case HPUpdate              => "HPUpdate"
    case ManaChange            => "ManaChange"
    case ManaUpdate            => "ManaUpdate"
    case Stamina               => "Stamina"
    case ExpUpdate             => "ExpUpdate"
    case LevelUpdate           => "LevelUpdate"
    case SkillUpdate           => "SkillUpdate"
    case SkillUpdate2          => "SkillUpdate2"
    case ResetSkill            => "ResetSkill"
    // Targeting
    case TargetMouse           => "TargetMouse"
    case TargetCommand         => "TargetCommand"
    case Assist                => "Assist"
    // Spells
    case CastSpell             => "CastSpell"
    case BeginCast             => "BeginCast"
    case InterruptCast         => "InterruptCast"
    case MemorizeSpell         => "MemorizeSpell"
    case SwapSpell             => "SwapSpell"
    case Buff                  => "Buff"
    case DeleteSpell           => "DeleteSpell"
    case Charm                 => "Charm"
    case Translocate           => "Translocate"
    case Sacrifice             => "Sacrifice"
    case TGB                   => "TGB"
    // Items / Inventory
    case CharInventory         => "CharInventory"
    case ItemPacket            => "ItemPacket"
    case ItemLinkResponse      => "ItemLinkResponse"
    case MerchantItemPacket    => "MerchantItemPacket"
    case TradeItemPacket       => "TradeItemPacket"
    case LootItemPacket        => "LootItemPacket"
    case ObjectItemPacket      => "ObjectItemPacket"
    case SummonedItem          => "SummonedItem"
    case ContainerPacket       => "ContainerPacket"
    case BookPacket            => "BookPacket"
    case MoveItem              => "MoveItem"
    case DeleteCharge          => "DeleteCharge"
    case MoneyUpdate           => "MoneyUpdate"
    case MoneyOnCorpse         => "MoneyOnCorpse"
    case MoveCoin              => "MoveCoin"
    case Split                 => "Split"
    case Key                   => "Key"
    // Loot / Corpse
    case LootRequest           => "LootRequest"
    case LootItem              => "LootItem"
    case LootComplete          => "LootComplete"
    case EndLootRequest        => "EndLootRequest"
    case CorpseDrag            => "CorpseDrag"
    case CorpsePosition        => "CorpsePosition"
    case Consent               => "Consent"
    case ConsentResponse       => "ConsentResponse"
    // Zone Objects / Tradeskill
    case SpawnDoor             => "SpawnDoor"
    case GroundSpawn           => "GroundSpawn"
    case SendZonepoints        => "SendZonepoints"
    case ClickDoor             => "ClickDoor"
    case MoveDoor              => "MoveDoor"
    case UpdateDoor            => "UpdateDoor"
    case DespawnDoor           => "DespawnDoor"
    case ClickObject           => "ClickObject"
    case ClickObjectAction     => "ClickObjectAction"
    case ClearObject           => "ClearObject"
    case TradeSkillCombine     => "TradeSkillCombine"
    // Environment / Transport
    case Weather               => "Weather"
    case Weather2              => "Weather2"
    case BoardBoat             => "BoardBoat"
    case LeaveBoat             => "LeaveBoat"
    case ControlBoat           => "ControlBoat"
    // Zone Movement / Logout
    case ZoneChange            => "ZoneChange"
    case RequestClientZoneChange => "RequestClientZoneChange"
    case SaveOnZoneReq         => "SaveOnZoneReq"
    case Camp                  => "Camp"
    case Logout                => "Logout"
    case LogoutReply           => "LogoutReply"
    case LogoutWorld           => "LogoutWorld"
    // Social
    case WhoAllRequest         => "WhoAllRequest"
    case WhoAllResponse        => "WhoAllResponse"
    case FriendsWho            => "FriendsWho"
    case RandomReq             => "RandomReq"
    case RandomReply           => "RandomReply"
    case LFGCommand            => "LFGCommand"
    case Bug                   => "Bug"
    case Feedback              => "Feedback"
    case Report                => "Report"
    // Group / Raid
    case GroupDisband          => "GroupDisband"
    case GroupInvite           => "GroupInvite"
    case GroupInvite2          => "GroupInvite2"
    case GroupFollow           => "GroupFollow"
    case GroupCancelInvite     => "GroupCancelInvite"
    case GroupUpdate           => "GroupUpdate"
    case RaidInvite            => "RaidInvite"
    case RaidUpdate            => "RaidUpdate"
    // Guild
    case GuildMOTD             => "GuildMOTD"
    case SetGuildMOTD          => "SetGuildMOTD"
    case GuildInvite           => "GuildInvite"
    case GuildInviteAccept     => "GuildInviteAccept"
    case GuildRemove           => "GuildRemove"
    case GuildDelete           => "GuildDelete"
    case GuildLeader           => "GuildLeader"
    case GuildPeace            => "GuildPeace"
    case GuildWar              => "GuildWar"
    case GuildAdded            => "GuildAdded"
    case GetGuildsList         => "GetGuildsList"
    // Trading
    case TradeRequest          => "TradeRequest"
    case TradeRequestAck       => "TradeRequestAck"
    case TradeAcceptClick      => "TradeAcceptClick"
    case TradeCoins            => "TradeCoins"
    case TradeMoneyUpdate      => "TradeMoneyUpdate"
    case FinishTrade           => "FinishTrade"
    case CancelTrade           => "CancelTrade"
    case TradeReset            => "TradeReset"
    case TradeRefused          => "TradeRefused"
    // Merchant / Bazaar
    case ShopRequest           => "ShopRequest"
    case ShopPlayerBuy         => "ShopPlayerBuy"
    case ShopPlayerSell        => "ShopPlayerSell"
    case ShopEnd               => "ShopEnd"
    case ShopEndConfirm        => "ShopEndConfirm"
    case ShopDelItem           => "ShopDelItem"
    case ShopInventoryPacket   => "ShopInventoryPacket"
    case BecomeTrader          => "BecomeTrader"
    case TraderShop            => "TraderShop"
    case Trader                => "Trader"
    case TraderBuy             => "TraderBuy"
    case BazaarSearch          => "BazaarSearch"
    // Duel
    case RequestDuel           => "RequestDuel"
    case DuelResponse          => "DuelResponse"
    case DuelResponse2         => "DuelResponse2"
    // Resurrect
    case RezzRequest           => "RezzRequest"
    case RezzAnswer            => "RezzAnswer"
    case RezzComplete          => "RezzComplete"
    // Class Skills
    case FeignDeath            => "FeignDeath"
    case Mend                  => "Mend"
    case Track                 => "Track"
    case Fishing               => "Fishing"
    case InstillDoubt          => "InstillDoubt"
    case ApplyPoison           => "ApplyPoison"
    case Begging               => "Begging"
    case SafeFallSuccess       => "SafeFallSuccess"
    case Hide                  => "Hide"
    case Sneak                 => "Sneak"
    case CancelSneakHide       => "CancelSneakHide"
    case SenseTraps            => "SenseTraps"
    case DisarmTraps           => "DisarmTraps"
    case PickPocket            => "PickPocket"
    case Disarm                => "Disarm"
    // AA
    case AAAction              => "AAAction"
    case RespondAA             => "RespondAA"
    case AAExpUpdate           => "AAExpUpdate"
    // Pet
    case PetCommands           => "PetCommands"
    // Inspect
    case InspectRequest        => "InspectRequest"
    case InspectAnswer         => "InspectAnswer"
    // GM / Training
    case GMServers             => "GMServers"
    case GMZoneRequest         => "GMZoneRequest"
    case GMZoneRequest2        => "GMZoneRequest2"
    case GMHideMe              => "GMHideMe"
    case GMGoto                => "GMGoto"
    case GMSummon              => "GMSummon"
    case GMKick                => "GMKick"
    case GMKill                => "GMKill"
    case GMToggle              => "GMToggle"
    case GMFind                => "GMFind"
    case GMDelCorpse           => "GMDelCorpse"
    case GMSearchCorpse        => "GMSearchCorpse"
    case GMEmoteZone           => "GMEmoteZone"
    case GMNameChange          => "GMNameChange"
    case GMLastName            => "GMLastName"
    case GMBecomeNPC           => "GMBecomeNPC"
    case GMTraining            => "GMTraining"
    case GMEndTraining         => "GMEndTraining"
    case GMTrainSkill          => "GMTrainSkill"
    case GMEndTrainingResponse => "GMEndTrainingResponse"
    // Petition
    case Petition              => "Petition"
    case PetitionCheckout      => "PetitionCheckout"
    case PetitionCheckIn       => "PetitionCheckIn"
    case PetitionDelete        => "PetitionDelete"
    case DeletePetition        => "DeletePetition"
    case PetitionRefresh       => "PetitionRefresh"
    case PetitionViewPetition  => "PetitionViewPetition"
    // SoulMark
    case SoulMarkAdd           => "SoulMarkAdd"
    case SoulMarkList          => "SoulMarkList"
    case SoulMarkUpdate        => "SoulMarkUpdate"
    // Sound
    case PlaySound             => "PlaySound"
    // Misc
    case Save                  => "Save"
    case Jump                  => "Jump"
    case SetRunMode            => "SetRunMode"
    case Consume               => "Consume"
    case ReadBook              => "ReadBook"
    case Forage                => "Forage"
    case SenseHeading          => "SenseHeading"
    case Bind_Wound            => "Bind_Wound"
    case SafePoint             => "SafePoint"
    case SendExpZonein         => "SendExpZonein"
    case Reward                => "Reward"
    case ExploreUnknown        => "ExploreUnknown"
    case other                 => f"Unknown(0x${other & 0xFFFF}%04x)"
