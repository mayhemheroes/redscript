use std::borrow::Cow;

use hashbrown::HashSet;

#[derive(Debug, Clone)]
pub struct TypeFlags {
    fully_defined: HashSet<Cow<'static, str>>,
    never_ref: HashSet<Cow<'static, str>>,
    mixed_ref: HashSet<Cow<'static, str>>,
}

impl TypeFlags {
    pub fn register_fully_defined(&mut self, name: impl Into<Cow<'static, str>>) {
        self.fully_defined.insert(name.into());
    }

    pub fn register_never_ref(&mut self, name: impl Into<Cow<'static, str>>) {
        self.never_ref.insert(name.into());
    }

    pub fn register_mixed_ref(&mut self, name: impl Into<Cow<'static, str>>) {
        self.mixed_ref.insert(name.into());
    }

    pub fn is_fully_defined(&self, name: &str) -> bool {
        self.fully_defined.contains(name)
    }

    pub fn is_never_ref(&self, name: &str) -> bool {
        self.never_ref.contains(name)
    }

    pub fn is_mixed_ref(&self, name: &str) -> bool {
        self.mixed_ref.contains(name)
    }
}

impl Default for TypeFlags {
    fn default() -> Self {
        Self {
            fully_defined: FULLY_DEFINED_TYPES
                .iter()
                .map(|s| Cow::Borrowed(*s))
                .collect(),
            never_ref: NEVER_REF_TYPES.iter().map(|s| Cow::Borrowed(*s)).collect(),
            mixed_ref: MIXED_REF_TYPES.iter().map(|s| Cow::Borrowed(*s)).collect(),
        }
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
