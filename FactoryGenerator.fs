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

module DeviceLoader =
    let getAlloySmelterBuildRecipe() =
        let assetName =  "base/alloy_smelter.device"
        let assetId =
            match AssetLibrary.Current.TryGetAssetId assetName with
            | false, _ -> failwith "This shouldn't happen, we did not find the alloy smelter recipe!"
            | true, id -> id
        BuildingRecipeManager.Current.Get assetId

module RegionHelper =
    let getUnlockedRegions =
        let list = System.Collections.Generic.List()
        RegionHelper.GetUnlockedRegions(list)
        list

module Utils =

    let tryExecuteButAbortAfter maxTries fn =
        let rec tryIt i =
            if maxTries <= i then false
            else
                if fn i then true
                else tryIt (i + 1)
        tryIt 0

        
    let random = QuickRandom()

    let inline randomNextXyz min max = Xyz(random.RangeInt(min, max), random.RangeInt(min, max), random.RangeInt(min, max))

type FactoryGeneratorManager() as this =
    inherit Manager<FactoryGeneratorManager>() 

    let mutable didAlreadyRun = false

    let logger = VoxelTycoon.Logger()

    let noopNotification =
        { new Notifications.INotificationAction with
              member _.Act(): unit = ()
              member _.Read(reader: StateBinaryReader): unit = ()
              member _.Write(writer: StateBinaryWriter): unit = () }


    member val Enabled = true with get, set

    override self.OnLateUpdate() =
        base.OnLateUpdate()

        if this.Enabled then
            if not didAlreadyRun then
                // TODO: Implement me!
                let companies = CompanyManager.Current.GetAll().ToList()
                let actualCompany = companies |> Seq.head
                let region = RegionHelper.getUnlockedRegions |> Seq.head
                Cities.CityManager.Current.Cities |> ListUtils.iter(fun city ->
                    if city.Region = region then
                        try
                        
                            let cityPos = city.GetSurfacePosition()
                            let recipe = DeviceLoader.getAlloySmelterBuildRecipe()
                            let building = recipe.Instantiate()
                            let success =
                                Utils.tryExecuteButAbortAfter 500 (fun i ->
                                    let randomPos = cityPos + Utils.randomNextXyz -40 +40
                                    if CanBuildHelper.CanBuildCityBuilding(randomPos, building.Size, region) then
                                        building.Build randomPos
                                        logger.Log($"Success building alloy smelter after {i} tries")
                                        true
                                    else false
                                )
                            if success then
                                Notifications.NotificationManager.Current.Push("building built!", "building built!", noopNotification) |> ignore
                                logger.Log("BUILDING BUILD")
                            else
                                let msg = $"Building this building failed after 100 tries"
                                Notifications.NotificationManager.Current.PushCritical("That failed!", msg, noopNotification) |> ignore
                                logger.LogError msg
                        with ex ->
                            let msg = $"Building this building failed, {ex}"
                            let _ = Notifications.NotificationManager.Current.PushCritical("That failed!", msg, noopNotification)
                            logger.LogError msg
                        ()
                )
                didAlreadyRun <- true

            else
                // TODO: Implement path which allows regeneration.
                ()

            ()


type FactoryGeneratorModSettingsTool() =
    let toogleHotkey = Hotkey KeyCode.Z
    let mutable toogleHotkeyPanelItem: HotkeyPanelItem option = None

    
    interface ITool with
        override _.Activate() =
            toogleHotkeyPanelItem <- Some <| HotkeyPanel.Current.Add("").AddKey(toogleHotkey)
        override _.OnUpdate() =
            if ToolHelper.IsHotkeyDown(toogleHotkey) then
                FactoryGeneratorManager.Current.Enabled <- not <| FactoryGeneratorManager.Current.Enabled

            toogleHotkeyPanelItem
            |> Option.iter (fun item -> item.SetCaption(if FactoryGeneratorManager.Current.Enabled then "Enabled" else "Disabled"))

            false

        override _.Deactivate _ =
            HotkeyPanel.Current.Clear()
            true


type FactoryGeneratorMod() =
    inherit Mod()

    override _.OnGameStarting() =
        FactoryGeneratorManager.Initialize()
        ()
    override _.OnGameStarted() =
        Toolbar.Current.AddButton(FontIcon.Ketizoloto(I.Settings1), "Factory Generator settings", ToolToolbarAction(fun () -> FactoryGeneratorModSettingsTool() :> _));
        ()
    override _.Read(reader: StateBinaryReader) =
        ()
    override _.Write(writer: StateBinaryWriter) =
        ()
