module Foo.Bar

open VoxelTycoon
open VoxelTycoon.Game.UI.ModernUI
open VoxelTycoon.Modding
open VoxelTycoon.Serialization
open VoxelTycoon.UI

open VoxelTycoon.Tracks
open UnityEngine
open VoxelTycoon.Game.UI
open VoxelTycoon.Tools
open VoxelTycoon.Buildings.AssetLoading
open VoxelTycoon.AssetLoading
open VoxelTycoon.Buildings

open FSharpPlus
open VoxelTycoon.Tools.Builder

module RegionHelper =
    let getUnlockedRegions =
        let list = System.Collections.Generic.List()
        RegionHelper.GetUnlockedRegions(list)
        list

module Logging =
    let getLoggerForClass (_: 'T) =
        VoxelTycoon.Logger(typedefof<'T>.ToString ())

module Utils =

    let tryExecuteButAbortAfter maxTries fn =
        let rec tryIt i =
            if maxTries < i then false
            else if fn i then true
            else tryIt (i + 1)

        tryIt 0


    let random = QuickRandom()

    let inline randomNextXyz min max =
        Xyz(random.RangeInt(min, max), random.RangeInt(min, max), random.RangeInt(min, max))

[<CLIMutable>]
type ModData = { didAlreadyRun: bool }

let builderToolBuild (recipe: ConveyorOperatorRecipe) pos rotation (logger: VoxelTycoon.Logger) =
    let roadHandler = BuilderToolRoadHandler()
    let railHandler = BuilderToolRailHandler()
    let conveyorHandler = BuilderToolConveyorHandler()
    let pipeHandler = BuilderToolPipeHandler()

    let handlers : BuilderToolHandler list =
        [ roadHandler :?> _
          railHandler :?> _
          conveyorHandler :?> _
          pipeHandler :?> _ ]

    let createGhost (rotation: BuildingRotation) =
        let ghost =
            UnityEngine.Object.Instantiate<Building>(recipe.Building)

        ghost.SetRotation rotation
        ghost

    let onBuildingGhost ghost =
        for handler in handlers do
            handler.OnBuildingGhost ghost

    let onValidated valid ghost =
        for handler in handlers do
            handler.OnValidated(ghost, valid)

    let placeGhost (pos: Xyz) (ghost: Building) =
        ghost.Place(pos, true)

        for handler in handlers do
            handler.PlaceGhost ghost

    // TODO: Implement this later
    let _validateGhost _canBuild _price _hintPosition = ()

    // TODO: Implement this later
    let _flatten _preview = ()

    // TODO: Add setTint too

    // TODO: Use later
    let _removeObstacles obstacles =
        for (obstacle: Building) in obstacles do
            obstacle.Remove true

    let buildGhost (ghost: Building) pos =
        let spawnConnections =
            [| for conveyorOperatorConveyor in recipe.Conveyors do
                   match conveyorOperatorConveyor.SpawnConnection
                         |> Option.ofNullable with
                   | Some value ->
                       let track =
                           conveyorHandler.GetTrack conveyorOperatorConveyor

                       logger.Log $"{track}"
                       logger.Log $"{track.AssetId}"
                       //logger.Log $"{track.Type}"
                       //logger.Log $"{track.PriceMultiplier}"
                       let connection =
                           track.GetConnection value :> TrackConnection

                       yield connection
                   | _ -> () |]
        (ghost :?> ConveyorOperator).SpawnConnections <- spawnConnections
        ghost.Build(pos, null)
        ghost.Company <- Company.Current

        for handler in handlers do
            handler.BuildGhost ghost

    conveyorHandler.SetRecipes(
        recipe.Conveyors
        |> Array.map (fun a -> a :> BuildingRecipeConveyor)
    )

    let ghost = createGhost rotation
    placeGhost pos ghost
    onValidated true ghost
    onBuildingGhost ghost
    buildGhost ghost pos
    ghost

type FactoryGeneratorManager() as this =
    inherit Manager<FactoryGeneratorManager>()

    let logger = Logging.getLoggerForClass this

    let noopNotification =
        { new Notifications.INotificationAction with
            member _.Act() : unit = ()
            member _.Read(reader: StateBinaryReader) : unit = ()
            member _.Write(writer: StateBinaryWriter) : unit = () }

    member val Enabled = true with get, set

    member val CurrentSavegameData = { didAlreadyRun = false } with get, set

    override self.OnLateUpdate() =
        base.OnLateUpdate()

        if this.Enabled then
            if not this.CurrentSavegameData.didAlreadyRun then
                let alloySmelter =
                    let uri = "base/alloy_smelter.device"

                    match AssetLibrary.Current.TryGetAssetId uri with
                    | true, id ->
                        match LazyManager<BuildingRecipeManager>.Current.TryGet id with
                        | true, recipe -> recipe :?> ConveyorOperatorRecipe
                        | _ -> failwith "Didn't find building recipe for asset id"
                    | _ -> failwith "Didn't find asset id for alloy smelter device"

                let region =
                    RegionManager.Current.HomeRegion |> Option.ofObj

                match region with
                | None -> logger.Log(LogType.Warning, "Current region was not fetchable yet")
                | Some region ->
                    region.Cities
                    |> ListUtils.iter
                        (fun city ->
                            try
                                let cityPos = city.GetSurfacePosition()

                                monad {
                                    let! deviceRecipe =
                                        AssetLibrary.Current.Get<Recipes.Recipe>("base/iron_bar.recipe")
                                        |> Option.ofObj

                                    let success =
                                        Utils.tryExecuteButAbortAfter
                                            500
                                            (fun i ->
                                                let randomPos = cityPos + Utils.randomNextXyz -40 40

                                                if
                                                    CanBuildHelper.CanBuildCityBuilding
                                                        (
                                                            randomPos,
                                                            alloySmelter.Building.Size,
                                                            city.Region
                                                        )
                                                then
                                                    let device =
                                                        builderToolBuild
                                                            alloySmelter
                                                            randomPos
                                                            BuildingRotation.Rotate0
                                                            logger

                                                    match device with
                                                    | :? Device as device ->
                                                        logger.Log($"Device connectors: %A{device.SpawnConnections}")
                                                        device.Recipe <- deviceRecipe
                                                        logger.Log("recipe set")
                                                    | _ -> ()

                                                    logger.Log($"Success building alloy smelter after {i} tries")
                                                    true
                                                else
                                                    false)

                                    if success then
                                        Notifications.NotificationManager.Current.Push(
                                            "building built!",
                                            "building built!",
                                            noopNotification
                                        )
                                        |> ignore

                                        logger.Log("BUILDING BUILD")
                                    else
                                        let msg =
                                            $"Building this building failed after 100 tries"

                                        Notifications.NotificationManager.Current.PushCritical(
                                            "That failed!",
                                            msg,
                                            noopNotification
                                        )
                                        |> ignore

                                        logger.LogError msg
                                }
                                |> Option.defaultWith (fun () -> logger.Log("BUILDING FAILED"))
                            with ex ->
                                let msg = $"Building this building failed, {ex}"

                                let _ =
                                    Notifications.NotificationManager.Current.PushCritical(
                                        "That failed!",
                                        msg,
                                        noopNotification
                                    )

                                logger.LogError msg)

                this.CurrentSavegameData <-
                    { this.CurrentSavegameData with
                          didAlreadyRun = true }
            else
                logger.Log "Already executed"
                // TODO: Implement path which allows regeneration.
                ()

            ()


type FactoryGeneratorModSettingsTool() =
    let toogleHotkey = Hotkey KeyCode.Z
    let mutable toogleHotkeyPanelItem : HotkeyPanelItem option = None


    interface ITool with
        override _.Activate() =
            toogleHotkeyPanelItem <-
                Some
                <| HotkeyPanel.Current.Add("").AddKey(toogleHotkey)

        override _.OnUpdate() =
            if ToolHelper.IsHotkeyDown(toogleHotkey) then
                FactoryGeneratorManager.Current.Enabled <- not <| FactoryGeneratorManager.Current.Enabled

            toogleHotkeyPanelItem
            |> Option.iter
                (fun item ->
                    item.SetCaption(
                        if FactoryGeneratorManager.Current.Enabled then
                            "Enabled"
                        else
                            "Disabled"
                    ))

            false

        override _.Deactivate _ =
            HotkeyPanel.Current.Clear()
            true

type FactoryGeneratorMod() as self =
    inherit Mod()

    let jsonSerializer = Newtonsoft.Json.JsonSerializer.Create()
    let logger = Logging.getLoggerForClass self

    override _.OnGameStarting() =
        FactoryGeneratorManager.Initialize()
        logger.Log "Game started"
        ()

    override _.OnGameStarted() =
        Toolbar.Current.AddButton(
            FontIcon.Ketizoloto(I.Settings1),
            "Factory Generator settings",
            ToolToolbarAction(fun () -> FactoryGeneratorModSettingsTool() :> _)
        )

        ()

    override _.Read(reader: StateBinaryReader) =
        use streamReader =
            new System.IO.StreamReader(reader.Stream)

        try
            logger.Log("trying to read data")

            let data =
                jsonSerializer.Deserialize(streamReader, typeof<ModData>) :?> ModData

            logger.Log($"Successfully read data, content: {data}")
            FactoryGeneratorManager.Current.CurrentSavegameData <- data
        with
        | :? System.IO.EndOfStreamException
        | :? System.IO.InvalidDataException ->
            // ok no data saved yet
            // That operation is not really necessary but for completeness we still do it.
            FactoryGeneratorManager.Current.CurrentSavegameData <- { didAlreadyRun = false }
        | ex -> logger.Log(LogType.Exception, $"Reading data failed due to {ex}")

        ()

    override _.Write(writer: StateBinaryWriter) =
        use streamWriter =
            new System.IO.StreamWriter(writer.Stream)

        jsonSerializer.Serialize(streamWriter, FactoryGeneratorManager.Current.CurrentSavegameData, typeof<ModData>)
