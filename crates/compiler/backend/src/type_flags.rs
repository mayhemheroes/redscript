use std::borrow::Cow;

use bitfield_struct::bitfield;
use hashbrown::HashMap;

#[bitfield(u8)]
pub struct TypeFlags {
    pub is_fully_defined: bool,
    pub is_never_ref: bool,
    pub is_mixed_ref: bool,

    #[bits(5)]
    __: u8,
}

impl TypeFlags {
    pub fn is_mixed_or_never_ref(&self) -> bool {
        self.is_mixed_ref() || self.is_never_ref()
    }
}

#[derive(Debug, Clone)]
pub struct TypeFlagRegistry {
    map: HashMap<Cow<'static, str>, TypeFlags>,
}

impl TypeFlagRegistry {
    #[inline]
    pub fn get(&self, name: &str) -> TypeFlags {
        self.map.get(name).copied().unwrap_or_default()
    }

    pub fn entry_or_default_mut(&mut self, name: impl Into<Cow<'static, str>>) -> &mut TypeFlags {
        self.map.entry(name.into()).or_default()
    }
}

impl Default for TypeFlagRegistry {
    fn default() -> Self {
        let mut map = HashMap::<Cow<'static, str>, TypeFlags>::new();

        for &name in FULLY_DEFINED_TYPES {
            map.entry(Cow::Borrowed(name))
                .or_default()
                .set_is_fully_defined(true);
        }

        for &name in NEVER_REF_TYPES {
            map.entry(Cow::Borrowed(name))
                .or_default()
                .set_is_never_ref(true);
        }

        for &name in MIXED_REF_TYPES {
            map.entry(Cow::Borrowed(name))
                .or_default()
                .set_is_mixed_ref(true);
        }

        Self { map }
    }
}

const NEVER_REF_TYPES: &[&str] = &["ReactionData", "vehicleCinematicCameraShotGroup"];

const MIXED_REF_TYPES: &[&str] = &[
    "JournalEntryOverrideData",
    "StatusEffect",
    "StatusEffectBase",
    "vehicleCinematicCameraShot",
];

// generated with https://github.com/jac3km4/redscript-sealed-struct-dumper
// this is a list of native structs that are fully exposed to scripts
const FULLY_DEFINED_TYPES: &[&str] = &[
    "AIActiveCommandList",
    "AICommandNodeFunction",
    "AIDelegateAttrRef",
    "AIDelegateTaskRef",
    "AIScriptUtils",
    "AVSpawnPointsRequestResult",
    "ActionDisplayData",
    "ActionPrereqs",
    "AmmoData",
    "AttackDebugData",
    "AttackInitContext",
    "BinkVideoSummary",
    "Box",
    "BuffInfo",
    "CachedBoolValue",
    "CharacterCustomizationAttribute",
    "CharactersChain",
    "ChatBoxText",
    "Color",
    "CombatSpaceHelper",
    "ComputerUIData",
    "ContextDisplayData",
    "ControllerHit",
    "CustomQuestNotificationData",
    "DSSSpawnRequestResult",
    "DamageInfo",
    "DebugDrawer",
    "DelayID",
    "DialogChoiceHubs",
    "DismemberedLimbCount",
    "DropInstruction",
    "EffectData",
    "EffectDurationModifierScriptContext",
    "EffectExecutionScriptContext",
    "EffectInfo",
    "EffectPreloadScriptContext",
    "EffectProviderScriptContext",
    "EffectScriptContext",
    "EffectSingleFilterScriptContext",
    "EngineTime",
    "EntityGameInterface",
    "EntityRequestComponentsInterface",
    "EntityResolveComponentsInterface",
    "EnumNameToIndexCache",
    "FragmentBuilder",
    "Frustum",
    "GOGRewardPack",
    "GameInstance",
    "GenericDataContent",
    "GetActionsContext",
    "HDRColor",
    "HandIKDescriptionResult",
    "HandleWithValue",
    "HitRepresentationQueryResult",
    "HitShapeResult",
    "IKTargetRef",
    "IMappinData",
    "ImpactPointData",
    "InnerItemData",
    "InputHintGroupData",
    "InputTriggerState",
    "InteractionChoiceCaption",
    "InteractionChoiceData",
    "InteractionChoiceHubData",
    "InteractionChoiceMetaData",
    "InteractionLayerData",
    "InventoryItemAbility",
    "InventoryItemSortData",
    "JournalFactNameValue",
    "JournalRequestStateFilter",
    "KillInfo",
    "LevelUpData",
    "LightPreset",
    "ListChoiceData",
    "ListenerAction",
    "ListenerActionConsumer",
    "LocationInformation",
    "LookAtLimits",
    "LookAtPartRequest",
    "LookAtRef",
    "LootVisualiserControlWrapper",
    "MappinUIProfile",
    "MappinUIUtils",
    "MappinUtils",
    "Matrix",
    "MeasurementUtils",
    "MinigameProgramData",
    "MotionConstrainedTierDataParams",
    "MountingInfo",
    "MountingSlotId",
    "MovementParameters",
    "NPCstubData",
    "NarrationEvent",
    "NarrativePlateData",
    "NavigationFindPointResult",
    "NearestRoadFromPlayerInfo",
    "NewMappinID",
    "PhotoModeOptionGridButtonData",
    "PinInfo",
    "PlayerBioMonitor",
    "PrereqCheckData",
    "PrereqData",
    "PrereqParams",
    "PreventionSystemDebugData",
    "Quaternion",
    "RWLock",
    "Range",
    "RectF",
    "RegisterCooldownFromRecordRequest",
    "RegisterNewCooldownRequest",
    "RemoteControlDrivingUIData",
    "RestrictMovementArea",
    "SDOSink",
    "SEquipArea",
    "SEquipSlot",
    "SEquipmentSet",
    "SItemInfo",
    "SItemStack",
    "SItemStackRequirementData",
    "SLastUsedWeapon",
    "SLoadout",
    "SPartSlots",
    "SSlotActiveItems",
    "SSlotInfo",
    "SSlotVisualInfo",
    "SVisualTagProcessing",
    "ScanningTooltipElementData",
    "ScanningTooltipElementDef",
    "ScriptExecutionContext",
    "SecureFootingResult",
    "SlotWeaponData",
    "SnapshotResult",
    "Sphere",
    "SquadOrder",
    "StatViewData",
    "StateMachineIdentifier",
    "StateMachineInstanceData",
    "StatusEffectTDBPicker",
    "StimuliMergeInfo",
    "TDBID",
    "TS_TargetPartInfo",
    "TacticRatio",
    "TargetFilterTicket",
    "TargetSearchFilter",
    "TelemetryDamageDealt",
    "TelemetryEnemy",
    "TelemetryEnemyDown",
    "TelemetryInventoryItem",
    "TelemetryLevelGained",
    "TelemetryQuickHack",
    "TelemetrySourceEntity",
    "TraceResult",
    "Transform",
    "TrialHelper",
    "TutorialBracketData",
    "UIScreenDefinition",
    "UnlockableProgram",
    "Vector2",
    "Vector3",
    "Vector4",
    "VendorData",
    "VideoWidgetSummary",
    "VisionBlockerTypeFlags",
    "VisualizersInfo",
    "WeakspotPhysicalDestructionComponent",
    "WeakspotPhysicalDestructionProperties",
    "WeaponRosterInfo",
    "WidgetUtils",
    "WorkEntryId",
    "WrappedEntIDArray",
    "bbUIInteractionData",
    "gameGrenadeThrowQueryParams",
    "gamePendingSubtitles",
    "gameSaveLock",
    "gameStatDetailedData",
    "gameSuggestedDefenseValues",
    "gameVisionModeSystemRevealIdentifier",
    "gameinteractionsActiveLayerData",
    "gamemappinsSenseCone",
    "gameprojectileLaunchParams",
    "gameuiDetectionParams",
    "gameuiDriverCombatCrosshairReticleData",
    "gameuiGenericNotificationData",
    "gameuiMountedWeaponTarget",
    "gameuiPatchIntroPackage",
    "gameuiSwitchPair",
    "gameuiWeaponShootParams",
    "inkInputKeyData",
    "inkMargin",
    "inkScreenProjectionData",
    "inkWidgetLibraryReference",
    "questPaymentConditionData",
    "scnDialogDisplayString",
    "scnDialogLineData",
    "smartGunUISightParameters",
    "smartGunUITargetParameters",
    "vehicleUnmountPosition",
    "worldTrafficLaneRef",
];
