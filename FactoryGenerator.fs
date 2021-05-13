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

type FactoryGeneratorManager() as this =
    inherit Manager<FactoryGeneratorManager>() 

    let mutable _transforms: ResizeArray<Transform> option = None

    member val Enabled = true with get, set

    override self.OnLateUpdate() =
        base.OnLateUpdate()

        if this.Enabled then
            // TODO: Implement me!
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


type TheMod() =
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
