package opennorrath.network

/** Application opcodes for the EQ Mac zone server protocol.
  * Values from EQMacDocker/Server/utils/patches/patch_Mac.conf.
  * These are big-endian wire values (matching how OldPacket reads opcodes).
  */
object ZoneOpcodes:

  // --- Zone Entry Handshake ---
  val ZoneEntry: Short          = 0x2840.toShort  // Client → Zone: request zone entry
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

  // --- Movement ---
  val ClientUpdate: Short       = 0xf340.toShort  // Client → Zone: player position
  val MobUpdate: Short          = 0x9f40.toShort  // Zone → Client: entity position update

  // --- Combat ---
  val AutoAttack: Short         = 0x5141.toShort  // Client → Zone: toggle melee
  val AutoAttack2: Short        = 0x6141.toShort  // Client → Zone: secondary attack
  val Damage: Short             = 0x5840.toShort  // Zone → Client: damage message
  val Consider: Short           = 0x3741.toShort  // Bidirectional: consider
  val EnvDamage: Short          = 0x1e40.toShort  // Zone → Client: environmental damage

  // --- Appearance / Animation ---
  val SpawnAppearance: Short    = 0xf540.toShort  // Bidirectional: appearance changes
  val WearChange: Short         = 0x9240.toShort  // Zone → Client: equipment visual change
  val Animation: Short          = 0xa140.toShort  // Zone → Client: mob animation
  val Action: Short             = 0x4640.toShort  // Zone → Client: spell/combat action
  val Illusion: Short           = 0x9140.toShort  // Zone → Client: illusion (race change)
  val FaceChange: Short         = 0x2542.toShort  // Bidirectional: face change

  // --- Chat / Messages ---
  val ChannelMessage: Short     = 0x0741.toShort  // Bidirectional: chat message
  val SpecialMesg: Short        = 0x8041.toShort  // Zone → Client: special message
  val FormattedMessage: Short   = 0x3642.toShort  // Zone → Client: formatted message
  val Emote: Short              = 0x1540.toShort  // Bidirectional: emote text

  // --- HP / Mana / Stats ---
  val HPUpdate: Short           = 0xb240.toShort  // Zone → Client: HP update
  val ManaChange: Short         = 0x7f41.toShort  // Zone → Client: mana change
  val ManaUpdate: Short         = 0x1942.toShort  // Zone → Client: mana update
  val Stamina: Short            = 0x5741.toShort  // Zone → Client: hunger/thirst/endurance
  val ExpUpdate: Short          = 0x9941.toShort  // Zone → Client: experience change
  val LevelUpdate: Short        = 0x9841.toShort  // Zone → Client: level up
  val SkillUpdate: Short        = 0x8941.toShort  // Zone → Client: skill change

  // --- Targeting ---
  val TargetMouse: Short        = 0x6241.toShort  // Client → Zone: target by mouse click
  val TargetCommand: Short      = 0xfe41.toShort  // Client → Zone: target by command

  // --- Spells ---
  val CastSpell: Short          = 0x7e41.toShort  // Client → Zone: cast spell
  val BeginCast: Short          = 0xa940.toShort  // Zone → Client: begin cast animation
  val InterruptCast: Short      = 0x3542.toShort  // Zone → Client: interrupt cast
  val MemorizeSpell: Short      = 0x8241.toShort  // Client → Zone: mem/unmem spell
  val Buff: Short               = 0x3241.toShort  // Zone → Client: buff added/removed
  val DeleteSpell: Short        = 0x4a42.toShort  // Zone → Client: spell deleted from book

  // --- Items / Inventory ---
  val CharInventory: Short      = 0xf641.toShort  // Zone → Client: inventory data
  val ItemPacket: Short         = 0x6441.toShort  // Zone → Client: item data
  val MoveItem: Short           = 0x2c41.toShort  // Client → Zone: move item
  val MoneyUpdate: Short        = 0x0840.toShort  // Zone → Client: money change
  val MoneyOnCorpse: Short      = 0x5040.toShort  // Zone → Client: corpse money

  // --- Loot ---
  val LootRequest: Short        = 0x4e40.toShort  // Client → Zone: open corpse
  val LootItem: Short           = 0xa040.toShort  // Client → Zone: loot item
  val LootComplete: Short       = 0x4541.toShort  // Zone → Client: loot done
  val EndLootRequest: Short     = 0x4f40.toShort  // Client → Zone: close corpse

  // --- Zone Objects ---
  val SpawnDoor: Short          = 0xf741.toShort  // Zone → Client: door spawns
  val GroundSpawn: Short        = 0x2c40.toShort  // Zone → Client: ground items
  val SendZonepoints: Short     = 0xb440.toShort  // Zone → Client: zone lines
  val ClickDoor: Short          = 0x8d40.toShort  // Client → Zone: click door
  val MoveDoor: Short           = 0x8e40.toShort  // Zone → Client: door animation

  // --- Environment ---
  val Weather: Short            = 0x3641.toShort  // Zone → Client: weather change
  val Weather2: Short           = 0x8a41.toShort  // Zone → Client: weather alt

  // --- Zone Movement ---
  val ZoneChange: Short         = 0xa340.toShort  // Bidirectional: zone change
  val RequestClientZoneChange: Short = 0x4d41.toShort // Zone → Client: request zone
  val SaveOnZoneReq: Short      = 0x5541.toShort  // Client → Zone: save before zone

  // --- Camp / Logout ---
  val Camp: Short               = 0x0742.toShort  // Client → Zone: camp
  val Logout: Short             = 0x5041.toShort  // Bidirectional: logout
  val LogoutReply: Short        = 0x5941.toShort  // Zone → Client: logout confirmed

  // --- Social ---
  val WhoAllRequest: Short      = 0xf440.toShort  // Client → Zone: /who
  val WhoAllResponse: Short     = 0x0b20.toShort  // Zone → Client: /who results
  val RandomReq: Short          = 0xe741.toShort  // Client → Zone: /random
  val RandomReply: Short        = 0x1640.toShort  // Zone → Client: random result

  // --- Group ---
  val GroupDisband: Short       = 0x4440.toShort  // Bidirectional: disband group
  val GroupInvite: Short        = 0x3e20.toShort  // Client → Zone: invite
  val GroupFollow: Short        = 0x3d20.toShort  // Client → Zone: accept invite
  val GroupUpdate: Short        = 0x2620.toShort  // Zone → Client: group update

  // --- Guild ---
  val GuildMOTD: Short          = 0x0442.toShort  // Zone → Client: guild motd
  val SetGuildMOTD: Short       = 0x0342.toShort  // Client → Zone: set guild motd
  val GuildInvite: Short        = 0x1741.toShort  // Bidirectional
  val GuildRemove: Short        = 0x1941.toShort  // Client → Zone
  val GuildInviteAccept: Short  = 0x1841.toShort  // Client → Zone

  // --- Trading ---
  val TradeRequest: Short       = 0xd140.toShort  // Bidirectional
  val TradeAcceptClick: Short   = 0xda40.toShort  // Client → Zone
  val FinishTrade: Short        = 0xdc40.toShort  // Bidirectional
  val CancelTrade: Short        = 0xdb40.toShort  // Bidirectional

  // --- Merchant ---
  val ShopRequest: Short        = 0x0b40.toShort  // Client → Zone
  val ShopPlayerBuy: Short      = 0x3540.toShort  // Client → Zone
  val ShopPlayerSell: Short     = 0x2740.toShort  // Client → Zone
  val ShopEnd: Short            = 0x3740.toShort  // Bidirectional

  // --- Misc Actions ---
  val Save: Short               = 0x2e40.toShort  // Client → Zone
  val Jump: Short               = 0x2040.toShort  // Client → Zone
  val SetRunMode: Short         = 0x1f40.toShort  // Client → Zone
  val Consume: Short            = 0x5641.toShort  // Client → Zone: eat/drink
  val ReadBook: Short           = 0xce40.toShort  // Client → Zone
  val Forage: Short             = 0x9440.toShort  // Client → Zone
  val SenseHeading: Short       = 0x8741.toShort  // Client → Zone
  val Bind_Wound: Short         = 0x9340.toShort  // Client → Zone

  // --- Sound ---
  val PlaySound: Short          = 0x4840.toShort  // Zone → Client

  // --- Inspect ---
  val InspectRequest: Short     = 0xb540.toShort  // Client → Zone
  val InspectAnswer: Short      = 0xb640.toShort  // Zone → Client

  // --- Pet ---
  val PetCommands: Short        = 0x4542.toShort  // Client → Zone

  // --- Safety ---
  val SafePoint: Short          = 0x2440.toShort  // Zone → Client
  val SendExpZonein: Short      = 0xd840.toShort  // Zone → Client

  def name(op: Short): String = op match
    case ZoneEntry             => "ZoneEntry"
    case PlayerProfile         => "PlayerProfile"
    case NewZone               => "NewZone"
    case ReqNewZone            => "ReqNewZone"
    case ReqClientSpawn        => "ReqClientSpawn"
    case TimeOfDay             => "TimeOfDay"
    case SetServerFilter       => "SetServerFilter"
    case DataRate              => "DataRate"
    case ZoneSpawns            => "ZoneSpawns"
    case NewSpawn              => "NewSpawn"
    case DeleteSpawn           => "DeleteSpawn"
    case Death                 => "Death"
    case ClientUpdate          => "ClientUpdate"
    case MobUpdate             => "MobUpdate"
    case AutoAttack            => "AutoAttack"
    case Damage                => "Damage"
    case Consider              => "Consider"
    case SpawnAppearance       => "SpawnAppearance"
    case WearChange            => "WearChange"
    case Animation             => "Animation"
    case Action                => "Action"
    case Illusion              => "Illusion"
    case ChannelMessage        => "ChannelMessage"
    case SpecialMesg           => "SpecialMesg"
    case FormattedMessage      => "FormattedMessage"
    case Emote                 => "Emote"
    case HPUpdate              => "HPUpdate"
    case ManaChange            => "ManaChange"
    case ManaUpdate            => "ManaUpdate"
    case Stamina               => "Stamina"
    case ExpUpdate             => "ExpUpdate"
    case LevelUpdate           => "LevelUpdate"
    case SkillUpdate           => "SkillUpdate"
    case TargetMouse           => "TargetMouse"
    case TargetCommand         => "TargetCommand"
    case CastSpell             => "CastSpell"
    case BeginCast             => "BeginCast"
    case InterruptCast         => "InterruptCast"
    case MemorizeSpell         => "MemorizeSpell"
    case Buff                  => "Buff"
    case CharInventory         => "CharInventory"
    case ItemPacket            => "ItemPacket"
    case MoveItem              => "MoveItem"
    case MoneyUpdate           => "MoneyUpdate"
    case LootRequest           => "LootRequest"
    case LootItem              => "LootItem"
    case LootComplete          => "LootComplete"
    case EndLootRequest        => "EndLootRequest"
    case SpawnDoor             => "SpawnDoor"
    case GroundSpawn           => "GroundSpawn"
    case SendZonepoints        => "SendZonepoints"
    case ClickDoor             => "ClickDoor"
    case MoveDoor              => "MoveDoor"
    case Weather               => "Weather"
    case ZoneChange            => "ZoneChange"
    case RequestClientZoneChange => "RequestClientZoneChange"
    case Camp                  => "Camp"
    case Logout                => "Logout"
    case LogoutReply           => "LogoutReply"
    case WhoAllRequest         => "WhoAllRequest"
    case WhoAllResponse        => "WhoAllResponse"
    case GroupDisband          => "GroupDisband"
    case GroupInvite           => "GroupInvite"
    case GroupFollow           => "GroupFollow"
    case GroupUpdate           => "GroupUpdate"
    case TradeRequest          => "TradeRequest"
    case FinishTrade           => "FinishTrade"
    case CancelTrade           => "CancelTrade"
    case ShopRequest           => "ShopRequest"
    case ShopPlayerBuy         => "ShopPlayerBuy"
    case ShopPlayerSell        => "ShopPlayerSell"
    case ShopEnd               => "ShopEnd"
    case Save                  => "Save"
    case Jump                  => "Jump"
    case Consume               => "Consume"
    case PlaySound             => "PlaySound"
    case InspectRequest        => "InspectRequest"
    case InspectAnswer         => "InspectAnswer"
    case PetCommands           => "PetCommands"
    case SafePoint             => "SafePoint"
    case SendExpZonein         => "SendExpZonein"
    case EnvDamage             => "EnvDamage"
    case FaceChange            => "FaceChange"
    case other                 => f"Unknown(0x${other & 0xFFFF}%04x)"
