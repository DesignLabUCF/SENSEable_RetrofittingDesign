# Facade Retrofitting Embodied Design (FRED)

**TODO** description

For our full paper, see **TODO**.

| [SENSEable Design Lab](https://sdl.eecs.ucf.edu/) | [UCF Modeling & Simulation](https://www.ist.ucf.edu/)  |
|--------|--------|
|    ![SENSEableDesign Lab](GitHubGraphics/SENSEable.png)    |    ![UCF](GitHubGraphics/UCF.png)    |
|   |   |


## Toolset

This project was designed in **Unreal Engine 4.25** for the **Microsoft HoloLens 2** and currently is only compatible with it.

**TODO** installation instructions

**TODO** adaptation instructions

## Current Application Workflow

The study administrator menu is accessible by facing the right palm up towards the camera while flat.

1. Launch application.
2. Scan any QR code 25 times.
3. Give headset to participant.
4. For participant: On the user interface, complete the training module.
5. For participant: Explore design options.
6. Manually record the participant's choice externally.

## Data Processing

When run as a standalone program, subject data is saved as an Unreal log file. When run on desktop or using HoloLens remote connection, subject data is written to the Unreal log and must be copy-pasted to a new .log file using a text editor. 

Data processing is primarly done using **Python 3.8.10**. A description of the directory's workflow and organization is found in [*Data*](Data) folder.

## Notes

#### Eye-Tracking
Must be enabled within HoloLens 2 after application installations. Settings > Privacy > Eye Tracker > Choose which apps

#### AR-Desktop
To to swap between desktop and AR versions:
* Change the game mode in project settings to desired variant
* Change the boolean variable in the main level blueprint
* Add BP_AR actor to scene if AR, remove if not 
* Add BP_SimMenu_Extra actor to scene if AR, remove if not
* Set visibility of DesktopLight Actor to FALSE if AR, TRUE if not
* Desktop light is a point light at (110, 120, 180) with intensity 3.0 cd
