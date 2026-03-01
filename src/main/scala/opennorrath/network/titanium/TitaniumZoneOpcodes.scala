package opennorrath.network.titanium

import opennorrath.network.ZoneOpcodes

/** Application opcodes for the Titanium (PC) zone server protocol.
  * Values from EQEmu/utils/patches/patch_Titanium.conf.
  * These are little-endian wire values (EqStream uses LE opcodes).
  */
object TitaniumZoneOpcodes extends ZoneOpcodes:

  // Mac-only opcodes — not present in Titanium protocol
  val DataRate: Short           = 0.toShort  // Not used in Titanium

  // --- Zone Entry Handshake ---
  val ZoneEntry: Short          = 0x7213.toShort  // Client → Zone: request zone entry
  val PlayerProfile: Short      = 0x75df.toShort  // Zone → Client: full character profile (19588 bytes)
  val NewZone: Short            = 0x0920.toShort  // Zone → Client: zone metadata
  val ReqNewZone: Short         = 0x7ac5.toShort  // Client → Zone: request zone data
  val ReqClientSpawn: Short     = 0x0322.toShort  // Client → Zone: ready for spawns
  val TimeOfDay: Short          = 0x1580.toShort  // Zone → Client: game time
  val SetServerFilter: Short    = 0x6563.toShort  // Client → Zone: message filter

  // --- Spawns ---
  val ZoneSpawns: Short         = 0x2e78.toShort  // Zone → Client: all spawns in zone
  val NewSpawn: Short           = 0x1860.toShort  // Zone → Client: new entity spawned
  val DeleteSpawn: Short        = 0x55bc.toShort  // Zone → Client: entity removed
  val Death: Short              = 0x6160.toShort  // Zone → Client: entity death

  // --- Movement / Client Control ---
  val ClientUpdate: Short       = 0x14cb.toShort  // Client → Zone: player position

  // --- Combat ---
  val AutoAttack: Short         = 0x5e55.toShort  // Client → Zone: toggle melee
  val AutoAttack2: Short        = 0x0701.toShort  // Client → Zone: secondary attack
  val Damage: Short             = 0x5c78.toShort  // Zone → Client: damage message
  val Consider: Short           = 0x65ca.toShort  // Bidirectional: consider
  val ConsiderCorpse: Short     = 0x773f.toShort  // Bidirectional: consider corpse
  val EnvDamage: Short          = 0x31b3.toShort  // Zone → Client: environmental damage
  val Taunt: Short              = 0x5e48.toShort  // Client → Zone: taunt
  val Stun: Short               = 0x1e51.toShort  // Zone → Client: stun effect
  val CombatAbility: Short      = 0x5ee8.toShort  // Client → Zone: combat ability
  val Shielding: Short          = 0x3fe6.toShort  // Bidirectional: /shield

  // --- Appearance / Animation ---
  val SpawnAppearance: Short    = 0x7c32.toShort  // Bidirectional: appearance changes
  val PlayerStateAdd: Short     = 0x63da.toShort  // Zone → Client: player state flags (8B: uint32 spawnId, uint32 state)
  val MobRename: Short          = 0x0498.toShort  // Zone → Client: mob name change (200B: char[64] x3, uint32 x2)
  val WearChange: Short         = 0x7441.toShort  // Zone → Client: equipment visual change
  val Animation: Short          = 0x2acf.toShort  // Zone → Client: mob animation
  val Action: Short             = 0x497c.toShort  // Zone → Client: spell/combat action
  val Illusion: Short           = 0x448d.toShort  // Zone → Client: illusion (race change)
  val FaceChange: Short         = 0x0f8e.toShort  // Bidirectional: face change
  val Surname: Short            = 0x4668.toShort  // Bidirectional: surname
  val SetTitle: Short           = 0x1f22.toShort  // Bidirectional: title

  // --- Chat / Messages ---
  val ChannelMessage: Short     = 0x1004.toShort  // Bidirectional: chat message
  val SpecialMesg: Short        = 0x2372.toShort  // Zone → Client: special message
  val FormattedMessage: Short   = 0x5a48.toShort  // Zone → Client: formatted message
  val SimpleMessage: Short      = 0x673c.toShort  // Zone → Client: string table message (12B: uint32 id, uint32 color, uint32 unk)
  val Emote: Short              = 0x547a.toShort  // Bidirectional: emote text
  val YellForHelp: Short        = 0x61ef.toShort  // Client → Zone: /yell

  // --- HP / Mana / Stats ---
  val HPUpdate: Short           = 0x3bcf.toShort  // Zone → Client: HP update (self, 12 bytes)
  val MobHealth: Short          = 0x0695.toShort  // Zone → Client: mob HP % (3 bytes: uint16 spawnId, uint8 hp)
  val ManaChange: Short         = 0x4839.toShort  // Zone → Client: mana change
  val Stamina: Short            = 0x7a83.toShort  // Zone → Client: hunger/thirst/endurance
  val ExpUpdate: Short          = 0x5ecd.toShort  // Zone → Client: experience change
  val LevelUpdate: Short        = 0x6d44.toShort  // Zone → Client: level up
  val SkillUpdate: Short        = 0x6a93.toShort  // Zone → Client: skill change

  // --- Targeting ---
  val TargetMouse: Short        = 0x6c47.toShort  // Client → Zone: target by mouse click
  val TargetCommand: Short      = 0x1477.toShort  // Client → Zone: target by command
  val TargetHoTT: Short         = 0x6a12.toShort  // Zone → Client: target's target (uint32 spawnId)
  val Assist: Short             = 0x7709.toShort  // Client → Zone: /assist

  // --- Spells ---
  val CastSpell: Short          = 0x304b.toShort  // Client → Zone: cast spell
  val BeginCast: Short          = 0x3990.toShort  // Zone → Client: begin cast animation
  val InterruptCast: Short      = 0x0b97.toShort  // Zone → Client: interrupt cast
  val MemorizeSpell: Short      = 0x308e.toShort  // Client → Zone: mem/unmem spell
  val SwapSpell: Short          = 0x2126.toShort  // Client → Zone: swap spell gem slots
  val Buff: Short               = 0x6a53.toShort  // Zone → Client: buff added/removed
  val DeleteSpell: Short        = 0x4f37.toShort  // Zone → Client: spell deleted from book
  val Charm: Short              = 0x12e5.toShort  // Zone → Client: charm effect
  val Translocate: Short        = 0x8258.toShort  // Bidirectional: teleport confirmation
  val Sacrifice: Short          = 0x727a.toShort  // Client → Zone: sacrifice spell
  val TGB: Short                = 0x0c11.toShort  // Client → Zone: target group buff toggle

  // --- Items / Inventory ---
  val CharInventory: Short      = 0x5394.toShort  // Zone → Client: inventory data
  val ItemPacket: Short         = 0x3397.toShort  // Zone → Client: item data
  val ItemLinkResponse: Short   = 0x667c.toShort  // Zone → Client: item link response
  val MoveItem: Short           = 0x420f.toShort  // Client → Zone: move item
  val DeleteCharge: Short       = 0x1c4a.toShort  // Zone → Client: item charge consumed
  val MoneyUpdate: Short        = 0x267c.toShort  // Zone → Client: money change
  val MoneyOnCorpse: Short      = 0x7fe4.toShort  // Zone → Client: corpse money
  val MoveCoin: Short           = 0x7657.toShort  // Client → Zone: move coin between slots
  val Split: Short              = 0x4848.toShort  // Client → Zone: split coins with group
  val Key: Short                = 0x68c4.toShort  // Zone → Client: key ring update

  // --- Loot / Corpse ---
  val LootRequest: Short        = 0x6f90.toShort  // Client → Zone: open corpse
  val LootItem: Short           = 0x7081.toShort  // Client → Zone: loot item
  val LootComplete: Short       = 0x0a94.toShort  // Zone → Client: loot done
  val EndLootRequest: Short     = 0x2316.toShort  // Client → Zone: close corpse
  val CorpseDrag: Short         = 0x50c0.toShort  // Client → Zone: drag corpse
  val Consent: Short            = 0x1081.toShort  // Client → Zone: /consent
  val ConsentResponse: Short    = 0x6380.toShort  // Zone → Client: consent response

  // --- Zone Objects / Tradeskill ---
  val SpawnDoor: Short          = 0x4c24.toShort  // Zone → Client: door spawns
  val GroundSpawn: Short        = 0x0f47.toShort  // Zone → Client: ground items
  val SendZonepoints: Short     = 0x3eba.toShort  // Zone → Client: zone lines
  val ClickDoor: Short          = 0x043b.toShort  // Client → Zone: click door
  val MoveDoor: Short           = 0x700d.toShort  // Zone → Client: door animation
  val ClickObject: Short        = 0x3bc2.toShort  // Client → Zone: click world object
  val ClickObjectAction: Short  = 0x6937.toShort  // Zone → Client: object action response
  val ClearObject: Short        = 0x21ed.toShort  // Zone → Client: clear object
  val TradeSkillCombine: Short  = 0x0b40.toShort  // Client → Zone: tradeskill combine

  // --- Environment / Transport ---
  val Weather: Short            = 0x254d.toShort  // Zone → Client: weather change
  val BoardBoat: Short          = 0x4298.toShort  // Client → Zone: board boat
  val LeaveBoat: Short          = 0x67c9.toShort  // Client → Zone: leave boat
  val ControlBoat: Short        = 0x2c81.toShort  // Client → Zone: control boat

  // --- Zone Movement / Logout ---
  val ZoneChange: Short         = 0x5dd8.toShort  // Bidirectional: zone change
  val RequestClientZoneChange: Short = 0x7834.toShort // Zone → Client: request zone
  val SaveOnZoneReq: Short      = 0x1540.toShort  // Client → Zone: save before zone
  val Camp: Short               = 0x78c1.toShort  // Client → Zone: camp
  val Logout: Short             = 0x61ff.toShort  // Bidirectional: logout
  val LogoutReply: Short        = 0x3cdc.toShort  // Zone → Client: logout confirmed
  val PreLogoutReply: Short     = 0x711e.toShort  // Zone → Client: pre-logout ack (0 bytes, sent during zone/camp)

  // --- Social ---
  val WhoAllRequest: Short      = 0x5cdd.toShort  // Client → Zone: /who
  val WhoAllResponse: Short     = 0x757b.toShort  // Zone → Client: /who results
  val FriendsWho: Short         = 0x48fe.toShort  // Client → Zone: /friends who
  val RandomReq: Short          = 0x5534.toShort  // Client → Zone: /random
  val RandomReply: Short        = 0x6cd5.toShort  // Zone → Client: random result
  val LFGCommand: Short         = 0x68ac.toShort  // Client → Zone: /lfg
  val Bug: Short                = 0x7ac2.toShort  // Client → Zone: /bug
  val Feedback: Short           = 0x5306.toShort  // Client → Zone: /feedback
  val Report: Short             = 0x7f9d.toShort  // Client → Zone: /report

  // --- Group / Raid ---
  val GroupDisband: Short       = 0x0e76.toShort  // Bidirectional: disband group
  val GroupInvite: Short        = 0x1b48.toShort  // Client → Zone: invite
  val GroupInvite2: Short       = 0x12d6.toShort  // Client → Zone: invite (alt)
  val GroupFollow: Short        = 0x7bc7.toShort  // Client → Zone: accept invite
  val GroupCancelInvite: Short  = 0x1f27.toShort  // Client → Zone: decline invite
  val GroupUpdate: Short        = 0x2dd6.toShort  // Zone → Client: group update
  val RaidInvite: Short         = 0x5891.toShort  // Bidirectional: raid invite
  val RaidUpdate: Short         = 0x1f21.toShort  // Zone → Client: raid update

  // --- Guild ---
  val GuildMOTD: Short          = 0x475a.toShort  // Zone → Client: guild motd
  val SetGuildMOTD: Short       = 0x591c.toShort  // Client → Zone: set guild motd
  val GuildInvite: Short        = 0x18b7.toShort  // Bidirectional
  val GuildInviteAccept: Short  = 0x61d0.toShort  // Client → Zone
  val GuildRemove: Short        = 0x0179.toShort  // Client → Zone
  val GuildDelete: Short        = 0x6cce.toShort  // Client → Zone
  val GuildLeader: Short        = 0x12b1.toShort  // Client → Zone: set guild leader
  val GuildPeace: Short         = 0x215a.toShort  // Client → Zone: guild peace
  val GuildWar: Short           = 0x0c81.toShort  // Client → Zone: guild war

  // --- Trading ---
  val TradeRequest: Short       = 0x372f.toShort  // Bidirectional
  val TradeRequestAck: Short    = 0x4048.toShort  // Zone → Client: trade request acknowledged
  val TradeAcceptClick: Short   = 0x0065.toShort  // Client → Zone
  val TradeCoins: Short         = 0x34c1.toShort  // Client → Zone: set trade coins
  val FinishTrade: Short        = 0x6014.toShort  // Bidirectional
  val CancelTrade: Short        = 0x2dc1.toShort  // Bidirectional

  // --- Merchant / Bazaar ---
  val ShopRequest: Short        = 0x45f9.toShort  // Client → Zone
  val ShopPlayerBuy: Short      = 0x221e.toShort  // Client → Zone
  val ShopPlayerSell: Short     = 0x0e13.toShort  // Client → Zone
  val ShopEnd: Short            = 0x7e03.toShort  // Bidirectional
  val ShopEndConfirm: Short     = 0x20b2.toShort  // Zone → Client: shop end confirmed
  val ShopDelItem: Short        = 0x0da9.toShort  // Zone → Client: remove item from shop
  val BecomeTrader: Short       = 0x2844.toShort  // Client → Zone: become bazaar trader
  val TraderShop: Short         = 0x35e8.toShort  // Bidirectional: browse trader
  val Trader: Short             = 0x524e.toShort  // Bidirectional: trader actions
  val TraderBuy: Short          = 0x6dd8.toShort  // Client → Zone: buy from trader
  val BazaarSearch: Short       = 0x1ee9.toShort  // Client → Zone: bazaar search

  // --- Duel ---
  val RequestDuel: Short        = 0x28e1.toShort  // Client → Zone: /duel

  // --- Resurrect ---
  val RezzRequest: Short        = 0x1035.toShort  // Zone → Client: resurrection offered
  val RezzAnswer: Short         = 0x6219.toShort  // Client → Zone: accept/decline rez
  val RezzComplete: Short       = 0x4b05.toShort  // Zone → Client: resurrection complete

  // --- Class Skills ---
  val FeignDeath: Short         = 0x7489.toShort  // Client → Zone: feign death
  val Mend: Short               = 0x14ef.toShort  // Client → Zone: monk mend
  val Track: Short              = 0x5d11.toShort  // Client → Zone: ranger tracking
  val Fishing: Short            = 0x0b36.toShort  // Client → Zone: fishing
  val InstillDoubt: Short       = 0x389e.toShort  // Client → Zone: instill doubt
  val ApplyPoison: Short        = 0x0c2c.toShort  // Client → Zone: apply poison
  val Begging: Short            = 0x13e7.toShort  // Client → Zone: begging
  val SafeFallSuccess: Short    = 0x3b21.toShort  // Zone → Client: safe fall succeeded
  val Hide: Short               = 0x4312.toShort  // Client → Zone: hide
  val Sneak: Short              = 0x74e1.toShort  // Client → Zone: sneak
  val CancelSneakHide: Short    = 0x48c2.toShort  // Client → Zone: cancel sneak/hide
  val SenseTraps: Short         = 0x5666.toShort  // Client → Zone: sense traps
  val DisarmTraps: Short        = 0x1241.toShort  // Client → Zone: disarm traps
  val PickPocket: Short         = 0x2ad8.toShort  // Client → Zone: pick pocket
  val Disarm: Short             = 0x17d9.toShort  // Client → Zone: disarm

  // --- AA ---
  val AAAction: Short           = 0x0681.toShort  // Client → Zone: AA purchase/activate
  val RespondAA: Short          = 0x3af4.toShort  // Zone → Client: AA data
  val AAExpUpdate: Short        = 0x5f58.toShort  // Zone → Client: AA exp update

  // --- Pet ---
  val PetCommands: Short        = 0x10a1.toShort  // Client → Zone

  // --- Inspect ---
  val InspectRequest: Short     = 0x775d.toShort  // Client → Zone
  val InspectAnswer: Short      = 0x2403.toShort  // Zone → Client

  // --- GM / Training ---
  val GMServers: Short          = 0x3387.toShort  // Client → Zone: /servers
  val GMZoneRequest: Short      = 0x1306.toShort  // Client → Zone: /zone
  val GMZoneRequest2: Short     = 0x244c.toShort  // Client → Zone: /zone (alt)
  val GMHideMe: Short           = 0x15b2.toShort  // Client → Zone: /hideme
  val GMGoto: Short             = 0x1cee.toShort  // Bidirectional: /goto or death zone-to-bind
  val GMSummon: Short           = 0x1edc.toShort  // Client → Zone: /summon
  val GMKick: Short             = 0x692c.toShort  // Client → Zone: /kick
  val GMKill: Short             = 0x6980.toShort  // Client → Zone: /kill
  val GMToggle: Short           = 0x7fea.toShort  // Client → Zone: /toggle
  val GMFind: Short             = 0x5930.toShort  // Client → Zone: /find
  val GMDelCorpse: Short        = 0x0b2f.toShort  // Client → Zone: /delcorpse
  val GMSearchCorpse: Short     = 0x3c32.toShort  // Client → Zone: /searchcorpse
  val GMEmoteZone: Short        = 0x39f2.toShort  // Client → Zone: /emotezone
  val GMLastName: Short         = 0x23a1.toShort  // Client → Zone: last name change
  val GMBecomeNPC: Short        = 0x7864.toShort  // Client → Zone: become NPC
  val GMTraining: Short         = 0x238f.toShort  // Zone → Client: open trainer window
  val GMEndTraining: Short      = 0x613d.toShort  // Client → Zone: close trainer window
  val GMTrainSkill: Short       = 0x11d2.toShort  // Client → Zone: train skill

  // --- Petition ---
  val Petition: Short           = 0x251f.toShort  // Client → Zone: /petition
  val PetitionDelete: Short     = 0x5692.toShort  // GM: delete petition

  // --- Sound ---
  val PlaySound: Short          = 0x541e.toShort  // Zone → Client

  // --- Misc ---
  val Save: Short               = 0x736b.toShort  // Client → Zone
  val Jump: Short               = 0x0797.toShort  // Client → Zone
  val SetRunMode: Short         = 0x4aba.toShort  // Client → Zone
  val Consume: Short            = 0x77d6.toShort  // Client → Zone: eat/drink
  val ReadBook: Short           = 0x1496.toShort  // Client → Zone
  val Forage: Short             = 0x4796.toShort  // Client → Zone
  val SenseHeading: Short       = 0x05ac.toShort  // Client → Zone
  val Bind_Wound: Short         = 0x601d.toShort  // Client → Zone
  val SendExpZonein: Short      = 0x0587.toShort  // Zone → Client

  // --- Tribute ---
  val TributeUpdate: Short      = 0x5639.toShort  // Zone → Client: tribute status (48 bytes)
  val TributeTimer: Short       = 0x4665.toShort  // Zone → Client: tribute timer (4 bytes)

  // --- Unhandled but known (suppresses "Unknown" log spam) ---
  val SendAAStats: Short        = 0x5996.toShort  // Zone → Client: AA stats (0 bytes)
  val CompletedTasks: Short     = 0x76a2.toShort  // Zone → Client: completed tasks list
  val DzCompass: Short          = 0x28aa.toShort  // Zone → Client: dungeon zone compass
  val DzExpeditionLockoutTimers: Short = 0x7c12.toShort // Zone → Client: expedition lockouts

  def name(op: Short): String = op match
    // Zone Entry
    case ZoneEntry             => "ZoneEntry"
    case PlayerProfile         => "PlayerProfile"
    case NewZone               => "NewZone"
    case ReqNewZone            => "ReqNewZone"
    case ReqClientSpawn        => "ReqClientSpawn"
    case TimeOfDay             => "TimeOfDay"
    case SetServerFilter       => "SetServerFilter"
    // Spawns
    case ZoneSpawns            => "ZoneSpawns"
    case NewSpawn              => "NewSpawn"
    case DeleteSpawn           => "DeleteSpawn"
    case Death                 => "Death"
    // Movement
    case ClientUpdate          => "ClientUpdate"
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
    case Shielding             => "Shielding"
    // Appearance / Animation
    case SpawnAppearance       => "SpawnAppearance"
    case PlayerStateAdd        => "PlayerStateAdd"
    case MobRename             => "MobRename"
    case WearChange            => "WearChange"
    case Animation             => "Animation"
    case Action                => "Action"
    case Illusion              => "Illusion"
    case FaceChange            => "FaceChange"
    case Surname               => "Surname"
    case SetTitle              => "SetTitle"
    // Chat / Messages
    case ChannelMessage        => "ChannelMessage"
    case SpecialMesg           => "SpecialMesg"
    case FormattedMessage      => "FormattedMessage"
    case SimpleMessage         => "SimpleMessage"
    case Emote                 => "Emote"
    case YellForHelp           => "YellForHelp"
    // HP / Mana / Stats
    case HPUpdate              => "HPUpdate"
    case MobHealth             => "MobHealth"
    case ManaChange            => "ManaChange"
    case Stamina               => "Stamina"
    case ExpUpdate             => "ExpUpdate"
    case LevelUpdate           => "LevelUpdate"
    case SkillUpdate           => "SkillUpdate"
    // Targeting
    case TargetMouse           => "TargetMouse"
    case TargetCommand         => "TargetCommand"
    case TargetHoTT            => "TargetHoTT"
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
    case Consent               => "Consent"
    case ConsentResponse       => "ConsentResponse"
    // Zone Objects / Tradeskill
    case SpawnDoor             => "SpawnDoor"
    case GroundSpawn           => "GroundSpawn"
    case SendZonepoints        => "SendZonepoints"
    case ClickDoor             => "ClickDoor"
    case MoveDoor              => "MoveDoor"
    case ClickObject           => "ClickObject"
    case ClickObjectAction     => "ClickObjectAction"
    case ClearObject           => "ClearObject"
    case TradeSkillCombine     => "TradeSkillCombine"
    // Environment / Transport
    case Weather               => "Weather"
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
    case PreLogoutReply        => "PreLogoutReply"
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
    // Trading
    case TradeRequest          => "TradeRequest"
    case TradeRequestAck       => "TradeRequestAck"
    case TradeAcceptClick      => "TradeAcceptClick"
    case TradeCoins            => "TradeCoins"
    case FinishTrade           => "FinishTrade"
    case CancelTrade           => "CancelTrade"
    // Merchant / Bazaar
    case ShopRequest           => "ShopRequest"
    case ShopPlayerBuy         => "ShopPlayerBuy"
    case ShopPlayerSell        => "ShopPlayerSell"
    case ShopEnd               => "ShopEnd"
    case ShopEndConfirm        => "ShopEndConfirm"
    case ShopDelItem           => "ShopDelItem"
    case BecomeTrader          => "BecomeTrader"
    case TraderShop            => "TraderShop"
    case Trader                => "Trader"
    case TraderBuy             => "TraderBuy"
    case BazaarSearch          => "BazaarSearch"
    // Duel
    case RequestDuel           => "RequestDuel"
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
    case GMLastName            => "GMLastName"
    case GMBecomeNPC           => "GMBecomeNPC"
    case GMTraining            => "GMTraining"
    case GMEndTraining         => "GMEndTraining"
    case GMTrainSkill          => "GMTrainSkill"
    // Petition
    case Petition              => "Petition"
    case PetitionDelete        => "PetitionDelete"
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
    case SendExpZonein         => "SendExpZonein"
    // Tribute
    case TributeUpdate         => "TributeUpdate"
    case TributeTimer          => "TributeTimer"
    // Unhandled but known
    case SendAAStats           => "SendAAStats"
    case CompletedTasks        => "CompletedTasks"
    case DzCompass             => "DzCompass"
    case DzExpeditionLockoutTimers => "DzExpeditionLockoutTimers"
    case other                 => f"Unknown(0x${other & 0xFFFF}%04x)"
