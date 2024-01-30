import Rhino
import rhinoscriptsyntax as rs
import scriptcontext as sc
import Rhino.Input as ri
import Rhino.DocObjects as rd
import System.Guid, System.Drawing.Color


room_length = 21.67
room_width = 14.5
height = 9.0 # TODO get exact
corner_size = 1.58 # 19 inches
side_wall_indent = 1.54
mullion_width = 0.79
#mullion_width = 0.17
mullion_length = 0.30 #TODO get
#mullion_horizontal_height = 0.5
mullion_horizontal_height = 0.17
left_window_distance_from_front = 7.08
door_length = 0.75
door_height = height * 0.75
door_width = 3.5
door_distance_from_side_wall = 8.0
door_frame_size = 0.17 # 2 inch

drywall_frame_distance = 2.0 # 24 inch
drywall_frame_width = 0.17
drywall_frame_depth = 0.5
drywall_frame_offset = 0.05

offset_size = 0.05


def build_drywalls():
    walls = []
    points = []
    
    points.append([-side_wall_indent, left_window_distance_from_front, 0])
    points.append([0, left_window_distance_from_front, 0])
    points.append([0, 0, 0]) # bottom left
    points.append([corner_size, 0, 0])
    points.append([corner_size, -corner_size, 0])
    points.append([room_width - corner_size, -corner_size, 0])
    points.append([room_width - corner_size, 0, 0])
    points.append([room_width, 0, 0]) # bottom right
    points.append([room_width, room_length - (2 * corner_size), 0]) # top right
    points.append([room_width - corner_size, room_length - (2 * corner_size), 0])
    points.append([room_width - corner_size, room_length - corner_size + mullion_width, 0]) # right end point of back wall
    walls.append(build_wall(points))
    del points[:]
    
    points.append([corner_size, room_length - corner_size + mullion_width, 0]) # left end point of back wall
    points.append([corner_size, room_length - (2 * corner_size), 0])
    points.append([0 - side_wall_indent, room_length - (2 * corner_size), 0]) # top left
    walls.append(build_wall(points))
    del points[:]
    
    for wall in walls:
        rs.FlipSurface(wall, True)
        
    return walls
    
    
def build_wall(points):
    lines = get_line_sequence(points)
    curve = rs.JoinCurves(lines) 
    height_path = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, height])
    wall = rs.ExtrudeCurve(curve, height_path)
    
    rs.DeleteObjects(lines)
    rs.DeleteObject(curve)
    rs.DeleteObject(height_path)
    
    set_layer(wall, "drywall")
    
    return wall
    
    
def build_floor():
    points = []
    points.append([0, 0, 0]) # bottom left
    points.append([corner_size, 0, 0])
    points.append([corner_size, -corner_size, 0])
    points.append([room_width - corner_size, -corner_size, 0])
    points.append([room_width - corner_size, 0, 0])
    points.append([room_width, 0, 0]) # bottom right
    points.append([room_width, room_length - (2 * corner_size), 0]) # top right
    points.append([room_width - corner_size, room_length - (2 * corner_size), 0])
    points.append([room_width - corner_size, room_length - corner_size, 0]) # right end point of back wall
    points.append([room_width - corner_size, room_length - corner_size + mullion_width, 0])
    points.append([corner_size, room_length - corner_size + mullion_width, 0])    
    points.append([corner_size, room_length - corner_size, 0]) # left end point of back wall
    points.append([corner_size, room_length - (2 * corner_size), 0])
    points.append([0, room_length - (2 * corner_size), 0]) # top left
    points.append([-side_wall_indent, room_length - (2 * corner_size), 0]) # start mullion area
    points.append([-side_wall_indent, left_window_distance_from_front, 0])
    points.append([0, left_window_distance_from_front, 0]) # end mullion area
    points.append([0, 0, 0])
    
    lines = get_line_sequence(points)
    curve = rs.CloseCurve(rs.JoinCurves(lines), -1.0)
    #floor = rs.MeshPolyline(curve)
    
    rs.DeleteObjects(lines)
    rs.DeleteObject(curve)
    
    #set_layer(floor, "floor")
    #return floor


def build_ceiling():
    points = []
    points.append([0, 0, 0]) # bottom left
    points.append([corner_size, 0, 0])
    points.append([corner_size, -corner_size, 0])
    points.append([room_width - corner_size, -corner_size, 0])
    points.append([room_width - corner_size, 0, 0])
    points.append([room_width, 0, 0]) # bottom right
    points.append([room_width, room_length - (2 * corner_size), 0]) # top right
    points.append([room_width - corner_size, room_length - (2 * corner_size), 0])
    points.append([room_width - corner_size, room_length - corner_size, 0]) # right end point of back wall
    points.append([room_width - corner_size, room_length - corner_size + mullion_width, 0])
    points.append([corner_size, room_length - corner_size + mullion_width, 0])    
    points.append([corner_size, room_length - corner_size, 0]) # left end point of back wall
    points.append([corner_size, room_length - (2 * corner_size), 0])
    points.append([0, room_length - (2 * corner_size), 0]) # top left
    points.append([-side_wall_indent, room_length - (2 * corner_size), 0]) # start mullion area
    points.append([-side_wall_indent, left_window_distance_from_front, 0])
    points.append([0, left_window_distance_from_front, 0]) # end mullion area
    points.append([0, 0, 0])
    
    lines = get_line_sequence(points)
    curve = rs.CloseCurve(rs.JoinCurves(lines), -1.0)
    ceiling = rs.MeshPolyline(curve)
    rs.MoveObject(ceiling, [0, 0, height])
    
    rs.DeleteObjects(lines)
    rs.DeleteObject(curve)
    
    #ceiling = brep_to_mesh(ceiling)
    
    set_layer(ceiling, "ceiling")
    return ceiling


def build_windows():
    windows = []
    
    # Back Window
    points = []
    points.append([corner_size, room_length - corner_size + mullion_width, 0])
    points.append([room_width - corner_size, room_length - corner_size + mullion_width, 0])
    
    line = rs.AddLine(points[0], points[1])
    height_path = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, height])
    window = rs.ExtrudeCurve(line, height_path)
    rs.DeleteObjects(line)
    rs.DeleteObject(height_path)
    set_layer(window, "windows")
    windows.append(window)
    
    # Side window
    points = []
    #points.append([0, 0, 0])
    #points.append([0, room_length - (2 * corner_size), 0])
    points.append([-side_wall_indent, room_length - (2 * corner_size), 0])
    points.append([-side_wall_indent, left_window_distance_from_front, 0])
    points.append([0, left_window_distance_from_front, 0])
    
    line = rs.AddLine(points[0], points[1])
    height_path = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, height])
    window = rs.ExtrudeCurve(line, height_path)
    rs.FlipSurface(window, True) # Flip so it faces inward
    rs.DeleteObjects(line)
    rs.DeleteObject(height_path) 
    set_layer(window, "windows")
    windows.append(window)
    
    return windows


def build_mullions():
    mullions = []

    left_wall_length = (room_length - (2 * corner_size)) - left_window_distance_from_front
    side_wall_mullions = []
    # Side wall horizontal
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front, 0], left_wall_length, mullion_width, mullion_horizontal_height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front, (1.0/5.0) * height], left_wall_length, mullion_width, mullion_horizontal_height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front, (4.0/5.0) * height - mullion_horizontal_height], left_wall_length, mullion_width, mullion_horizontal_height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front, height - mullion_horizontal_height], left_wall_length, mullion_width, mullion_horizontal_height))
    
    # Side wall vertical
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front, 0], mullion_length, mullion_width, height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front + (1.0/8.0) * left_wall_length, 0], mullion_length, mullion_width, height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front + (7.0/16.0) * left_wall_length, 0], mullion_length, mullion_width, height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front + (9.0/16.0) * left_wall_length - mullion_length, 0], mullion_length, mullion_width, height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front + (7.0/8.0) * left_wall_length - mullion_length, 0], mullion_length, mullion_width, height))
    side_wall_mullions.append(build_box([-side_wall_indent, left_window_distance_from_front + left_wall_length - mullion_length, 0], mullion_length, mullion_width, height))
    
    side_wall_union = rs.BooleanUnion(side_wall_mullions, True)
    
    back_wall_mullions = []
    # Back wall horizontal
    back_wall_mullions.append(build_box([corner_size, room_length - corner_size, 0], mullion_width, room_width - (2.0 * corner_size), mullion_horizontal_height))
    back_wall_mullions.append(build_box([corner_size, room_length - corner_size, (1.0/5.0) * height], mullion_width, room_width - (2.0 * corner_size), mullion_horizontal_height))
    back_wall_mullions.append(build_box([corner_size, room_length - corner_size, (4.0/5.0) * height - mullion_horizontal_height], mullion_width, room_width - (2.0 * corner_size), mullion_horizontal_height))
    back_wall_mullions.append(build_box([corner_size, room_length - corner_size, height - mullion_horizontal_height], mullion_width, room_width - (2.0 * corner_size), mullion_horizontal_height))

    # Back wall vertical
    back_wall_mullions.append(build_box([corner_size, room_length - corner_size, 0], mullion_width, mullion_length, height))
    back_wall_mullions.append(build_box([corner_size + (1.0/8.0) * (room_width - 2 * corner_size), room_length - corner_size, 0], mullion_width, mullion_length, height))
    back_wall_mullions.append(build_box([corner_size + (7.0/16.0) * (room_width - 2 * corner_size), room_length - corner_size, 0], mullion_width, mullion_length, height))
    back_wall_mullions.append(build_box([corner_size + (9.0/16.0) * (room_width - 2 * corner_size) - mullion_length, room_length - corner_size, 0], mullion_width, mullion_length, height))
    back_wall_mullions.append(build_box([corner_size + (7.0/8.0) * (room_width - 2 * corner_size) - mullion_length, room_length - corner_size, 0], mullion_width, mullion_length, height))
    back_wall_mullions.append(build_box([corner_size + (room_width - 2 * corner_size) - mullion_length, room_length - corner_size, 0], mullion_width, mullion_length, height))
    
    back_wall_union = rs.BooleanUnion(back_wall_mullions, True)
    
    set_layer(side_wall_union, "mullions")
    set_layer(back_wall_union, "mullions")
    mullions.append(side_wall_union)
    mullions.append(back_wall_union)

    return mullions
    
    
def build_box(bottom_lower_left_point, length, width, height):
    bottom_lower_left_point = bottom_lower_left_point
    bottom_lower_right_point = [bottom_lower_left_point[0] + width, bottom_lower_left_point[1], bottom_lower_left_point[2]]
    bottom_upper_right_point = [bottom_lower_left_point[0] + width, bottom_lower_left_point[1], bottom_lower_left_point[2] + height]
    bottom_upper_left_point = [bottom_lower_left_point[0], bottom_lower_left_point[1], bottom_lower_left_point[2] + height]
    top_lower_left_point = [bottom_lower_left_point[0], bottom_lower_left_point[1] + length, bottom_lower_left_point[2]]
    top_lower_right_point = [bottom_lower_left_point[0] + width, bottom_lower_left_point[1] + length, bottom_lower_left_point[2]]
    top_upper_right_point = [bottom_lower_left_point[0] + width, bottom_lower_left_point[1] + length, bottom_lower_left_point[2] + height]
    top_upper_left_point = [bottom_lower_left_point[0], bottom_lower_left_point[1] + length, bottom_lower_left_point[2] + height]
    
    return rs.AddBox([bottom_lower_left_point, bottom_lower_right_point, bottom_upper_right_point, bottom_upper_left_point, top_lower_left_point, top_lower_right_point, top_upper_right_point, top_upper_left_point])


def build_door():
    bottom_left_corner_door = [door_distance_from_side_wall, -corner_size - (door_length / 2.0), 0]
    door = build_box(bottom_left_corner_door, door_length, door_width, door_height)
    
    bottom_left_corner_frame = [bottom_left_corner_door[0] - door_frame_size, bottom_left_corner_door[1], bottom_left_corner_door[2]]
    door_frame = build_box(bottom_left_corner_frame, door_length, door_width + (2.0 * door_frame_size), door_height + door_frame_size)
    door_for_carving_out_frame = build_box(bottom_left_corner_door, door_length, door_width, door_height)
    door_frame = rs.BooleanDifference(door_frame, door_for_carving_out_frame, True)
    
    set_layer(door, "door")
    set_layer(door_frame, "door_frame")
    
    return door


def build_drywall_frame():
    frame = []
    
    # Horizontal
    frame.append(build_box([room_width + drywall_frame_offset, 0, 0], room_length - (2.0 * corner_size), drywall_frame_depth, drywall_frame_width)) # bottom
    frame.append(build_box([room_width + drywall_frame_offset, 0, height / 2.0], room_length - (2.0 * corner_size), drywall_frame_depth, drywall_frame_width)) # middle
    frame.append(build_box([room_width + drywall_frame_offset, 0, height - drywall_frame_width], room_length - (2.0 * corner_size), drywall_frame_depth, drywall_frame_width)) # top
    
    # Vertical
    frame.append(build_box([room_width + drywall_frame_offset, 0, 0], drywall_frame_width, drywall_frame_depth, height)) # front
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance * 2.0, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance * 3.0, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance * 4.0, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance * 5.0, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance * 6.0, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance * 7.0, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, drywall_frame_distance * 8.0, 0], drywall_frame_width, drywall_frame_depth, height))
    frame.append(build_box([room_width + drywall_frame_offset, room_length - (corner_size * 2.0), 0], drywall_frame_width, drywall_frame_depth, height)) # not perfect but close enough
    
    frame_union = rs.BooleanUnion(frame, True)
    set_layer(frame_union, "frame")

    return frame_union


"""
IN: List of [x, y, z] points
OUT: List of lines created using those points
"""
def get_line_sequence(points):
    #TODO add check for empty points list
    
    lines = []
    previousPoint = points[0]
    for i in range(1, len(points)):
        currentPoint = points[i]
        lines.append(rs.AddLine(previousPoint, currentPoint))
        previousPoint = currentPoint
    
    return lines


def add_layer(layer_name):
    layer_index = sc.doc.Layers.Find(layer_name, True)
    if layer_index>=0:
        return Rhino.Commands.Result.Success
    
    layer_index = sc.doc.Layers.Add(layer_name, System.Drawing.Color.Black)
    if layer_index<0:
        print ("Unable to add", layer_name, "layer.")
        return Rhino.Commands.Result.Failure


def set_layer(obj, layer_name):
    if(rs.IsLayer(layer_name)):
        rs.ObjectLayer(obj, layer_name)
    else:
        add_layer(layer_name)


def inches_to_feet(inches):
    scale = rs.UnitScale(9, 8)
    return inches * scale


def feet_to_inches(feet):
    scale = rs.UnitScale(8, 9)
    return inches * scale


def brep_to_mesh(brep):
    meshes = Rhino.Geometry.Mesh.CreateFromBrep(brep, Rhino.Geometry.MeshingParameters.Default)
    if meshes == None or meshes.Length == 0:
        return Rhino.Commands.Result.Failure
        
    brepMesh = Rhino.Geometry.Mesh()
    for mesh in meshes:
        brepMesh.Append(mesh)

    return brepMesh


def get_obj_from_guid(guid):
    return rs.coercerhinoobject(guid)


print("Building P2-141 Conference Room...")
#walls = build_drywalls()
floor = build_floor()
#ceiling = build_ceiling()
#windows = build_windows()
#mullions = build_mullions()
#door = build_door()
#frame = build_drywall_frame()
print("Build complete.")