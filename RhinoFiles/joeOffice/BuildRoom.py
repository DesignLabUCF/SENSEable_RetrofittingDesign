import Rhino
import rhinoscriptsyntax as rs
import scriptcontext as sc
import Rhino.Input as ri
import Rhino.DocObjects as rd

def BuildMullionHorizontal(A,B,C,D):
   line1 = rs.AddLine(A, B)
   line2 = rs.AddLine(B, C)
   line3 = rs.AddLine(C, D)
   line4 = rs.AddLine(D, A)
   
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 107.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "mullion")
   
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

def BuildMullionVertical(A,B,C,D):
   line1 = rs.AddLine(A, B)
   line2 = rs.AddLine(B, C)
   line3 = rs.AddLine(C, D)
   line4 = rs.AddLine(D, A)
   
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   
   path1 = rs.AddLine([0.0, 0.0, 0.0],[155.0, 0.0, 0.0])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "mullion")
   
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

def BuildDoor(a):
    line1 = rs.AddLine([-4.75, -172.375, 0.0], [-4.75, -172.375, 83.0])
    line2 = rs.AddLine([-4.75, -172.375, 83.0], [28.75, -172.375, 83.0])
    line3 = rs.AddLine([28.75, -172.375, 83.0], [28.75, -172.375, 0.0])
    line4 = rs.AddLine([28.75, -172.375, 0.0], [-4.75, -172.375, 0.0])

    curve1 = rs.JoinCurves([line1,line2,line3,line4])


    path1 = rs.AddLine([-4.75, -172.375, 0.0],[-4.75, -170.375, 0.0])
    ext1 = rs.ExtrudeCurve(curve1,path1)
    rs.CapPlanarHoles(ext1)

    rs.ExplodePolysurfaces(ext1)
    rs.DeleteObjects([line1,line2,line3,line4, curve1, path1, ext1])
    
    
    rhobjs= [
      obj for obj in sc.doc.Objects.GetObjectList(rd.ObjectType.AnyObject)
      if obj.IsSelectable(True, False, False, False)]    
    rs.DeleteObjects([rhobjs[0], rhobjs[1],rhobjs[2],rhobjs[3],rhobjs[4]])
    if(a):
       rs.ObjectLayer(rhobjs[5], "door")
    else:
       return rhobjs[5]


def BuildWalls():
    line1 = rs.AddLine([0.0, -3.75, 0.0], [0.0, -7.875, 0.0])
    line2 = rs.AddLine([0.0, -7.875, 0.0], [-12.0, -7.875, 0.0])
    line3 = rs.AddLine([-11.5, -172.375, 0.0], [-12.0, -7.875, 0.0])
    line4 = rs.AddLine([-11.5, -172.375, 0.0], [167.5, -172.375, 0.0])
    line5 = rs.AddLine([168.0, -25.25, 0.0], [167.5, -172.375, 0.0])
    line6 = rs.AddLine([168.0, -25.25, 0.0], [155.0, -25.75, 0.0])
    line7 = rs.AddLine([155.0, -3.75, 0.0], [155.0, -25.75, 0.0])

    
    curve1 = rs.JoinCurves([line1,line2,line3,line4,line5,line6,line7])
    path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 107.75])
    ext1 = rs.ExtrudeCurve(curve1,path1)
    
    rs.DeleteObjects([line1,line2,line3,line4,line5,line6,line7, path1, curve1])

    rs.ObjectLayer(rs.BooleanDifference(ext1, BuildDoor(False), True) , "drywall")

def BuildTopBottom():
    line1 = rs.AddLine([0.0, 0.0, 0.0], [0.0, -7.875, 0.0])
    line2 = rs.AddLine([0.0, -7.875, 0.0], [-12.0, -7.875, 0.0])
    line3 = rs.AddLine([-11.5, -172.375, 0.0], [-12.0, -7.875, 0.0])
    line4 = rs.AddLine([-11.5, -172.375, 0.0], [167.5, -172.375, 0.0])
    line5 = rs.AddLine([168.0, -25.25, 0.0], [167.5, -172.375, 0.0])
    line6 = rs.AddLine([168.0, -25.25, 0.0], [155.0, -25.75, 0.0])
    line7 = rs.AddLine([155.0, 0.0, 0.0], [155.0, -25.75, 0.0])
    line8 = rs.AddLine([155.0, 0.0, 0.0], [0.0, 0.0, 0.0])

    
    curve1 = rs.JoinCurves([line1,line2,line3,line4,line5,line6,line7,line8])
    path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 107.75])
    ext1 = rs.ExtrudeCurve(curve1,path1)
    rs.CapPlanarHoles(ext1)
    
    rs.ExplodePolysurfaces(ext1)
    
    #rs.ObjectLayer(ext1, "drywall")
    rs.DeleteObjects([line1,line2,line3,line4,line5,line6,line7,line8,ext1, path1,  curve1])
    
   
    rhobjs= [
      obj for obj in sc.doc.Objects.GetObjectList(rd.ObjectType.AnyObject)
      if obj.IsSelectable(True, False, False, False)]    
    
    
    rs.ObjectLayer(rhobjs[0], "window")
    rs.ObjectLayer(rhobjs[8], "floor")
    rs.ObjectLayer(rhobjs[9], "ceiling")
    
    rs.DeleteObjects([rhobjs[1],rhobjs[2],rhobjs[3],rhobjs[4], rhobjs[5], rhobjs[6], rhobjs[7]])


def BuildMullions():
    BuildMullionHorizontal([0.0, 0.0, 0.0], [1.75, 0.0, 0.0], [1.75, -3.75, 0.0], [0.0, -3.75, 0.0])
    BuildMullionHorizontal([15.5, 0.0, 0.0], [17.25, 0.0, 0.0], [17.25, -3.75, 0.0], [15.5, -3.75, 0.0])
    BuildMullionHorizontal([71.25, 0.0, 0.0], [73.0, 0.0, 0.0], [73.0, -3.75, 0.0], [71.25, -3.75, 0.0])
    BuildMullionHorizontal([81.5, 0.0, 0.0], [83.25, 0.0, 0.0], [83.25, -3.75, 0.0], [81.5, -3.75, 0.0])
    BuildMullionHorizontal([137.25, 0.0, 0.0], [139.0, 0.0, 0.0], [139.0, -3.75, 0.0], [137.25, -3.75, 0.0])
    BuildMullionHorizontal([153.25, 0.0, 0.0], [155.0, 0.0, 0.0], [155.0, -3.75, 0.0], [153.25, -3.75, 0.0])
    BuildMullionVertical([0.0, 0.0, 0.0], [0.0, 0.0, 1.75], [0.0, -3.75, 1.75], [0.0,-3.75, 0.0] )
    BuildMullionVertical([0.0, 0.0, 107.75], [0.0, 0.0, 106.0], [0.0, -3.75, 106.0], [0.0,-3.75, 107.75] )
    BuildMullionVertical([0.0, 0.0, 86.25], [0.0, 0.0, 84.5], [0.0, -3.75, 84.5], [0.0,-3.75, 86.25] )
    BuildMullionVertical([0.0, 0.0, 21.5], [0.0, 0.0, 23.25], [0.0, -3.75, 23.25], [0.0,-3.75, 21.5] )



def BuildBookShelf():
   line1 = rs.AddLine([0.0,-0.75,0.0], [0.0,-35.25,0.0])
   line2 = rs.AddLine([0.0,-35.25,0.0], [-13.25,-35.25,0.0])
   line3 = rs.AddLine([-13.25,-35.25,0.0], [-13.25,-0.75,0.0])
   line4 = rs.AddLine([-13.25,-0.75,0.0], [0.0,-0.75,0.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 2.0])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])



   line1 = rs.AddLine([0.125,-35.25,0.0], [-14.0,-35.25,0.0])
   line2 = rs.AddLine([-14.0,-35.25,0.0], [-14.0,-36.0,0.0])
   line3 = rs.AddLine([-14.0,-36.0,0.0], [0.125,-36.0,0.0])
   line4 = rs.AddLine([0.125,-36.0,0.0], [0.125,-35.25,0.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 72.625])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

   line1 = rs.AddLine([0.125,-0.75,0.0], [-14.0,-0.75,0.0])
   line2 = rs.AddLine([-14.0,-0.75,0.0], [-14.0,0.0,0.0])
   line3 = rs.AddLine([-14.0,0.0,0.0], [0.125,0.0,0.0])
   line4 = rs.AddLine([0.125,0.0,0.0], [0.125,-0.75,0.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 72.625])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])
   
   line1 = rs.AddLine([-14.0,-0.75,0.0], [-13.25,-0.75,0.0])
   line2 = rs.AddLine([-13.25,-0.75,0.0], [-13.25,-35.25,0.0])
   line3 = rs.AddLine([-13.25,-35.25,0.0], [-14.0,-35.25,0.0])
   line4 = rs.AddLine([-14.0,-35.25,0.0], [-14.0,-0.75,0.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 72.5])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

   #shelves
   offset = 2.0
   line1 = rs.AddLine([0.0,-0.75,offset], [0.0,-35.25,offset])
   line2 = rs.AddLine([0.0,-35.25,offset], [-13.25,-35.25,offset])
   line3 = rs.AddLine([-13.25,-35.25,offset], [-13.25,-0.75,offset])
   line4 = rs.AddLine([-13.25,-0.75,offset], [0.0,-0.75,offset])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])
   
   offset = 2.0+0.75+13.25
   line1 = rs.AddLine([0.0,-0.75,offset], [0.0,-35.25,offset])
   line2 = rs.AddLine([0.0,-35.25,offset], [-13.25,-35.25,offset])
   line3 = rs.AddLine([-13.25,-35.25,offset], [-13.25,-0.75,offset])
   line4 = rs.AddLine([-13.25,-0.75,offset], [0.0,-0.75,offset])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])
   
   offset = 2.0+0.75+13.5*2
   line1 = rs.AddLine([0.0,-0.75,offset], [0.0,-35.25,offset])
   line2 = rs.AddLine([0.0,-35.25,offset], [-13.25,-35.25,offset])
   line3 = rs.AddLine([-13.25,-35.25,offset], [-13.25,-0.75,offset])
   line4 = rs.AddLine([-13.25,-0.75,offset], [0.0,-0.75,offset])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])
   
   offset = 2.0+0.75+13.5*3
   line1 = rs.AddLine([0.0,-0.75,offset], [0.0,-35.25,offset])
   line2 = rs.AddLine([0.0,-35.25,offset], [-13.25,-35.25,offset])
   line3 = rs.AddLine([-13.25,-35.25,offset], [-13.25,-0.75,offset])
   line4 = rs.AddLine([-13.25,-0.75,offset], [0.0,-0.75,offset])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])   

   offset = 2.0+0.75+13.75*4
   line1 = rs.AddLine([0.0,-0.75,offset], [0.0,-35.25,offset])
   line2 = rs.AddLine([0.0,-35.25,offset], [-13.25,-35.25,offset])
   line3 = rs.AddLine([-13.25,-35.25,offset], [-13.25,-0.75,offset])
   line4 = rs.AddLine([-13.25,-0.75,offset], [0.0,-0.75,offset])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1]) 
   
   offset = 2.0+0.75+13.8*5
   line1 = rs.AddLine([0.0,-0.75,offset], [0.0,-35.25,offset])
   line2 = rs.AddLine([0.0,-35.25,offset], [-13.25,-35.25,offset])
   line3 = rs.AddLine([-13.25,-35.25,offset], [-13.25,-0.75,offset])
   line4 = rs.AddLine([-13.25,-0.75,offset], [0.0,-0.75,offset])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.ObjectLayer(ext1, "bookshelf")
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])    


def BuildTable():
   rs.AddCylinder([0.0,0.0,28.875], 1.125, 21.0)
   rs.AddCylinder([0.0,0.0,0.0], 28.875, 2.0)
   
   line1 = rs.AddLine([16.0,0.375,0.0], [16.0,-0.375,0.0])
   line2 = rs.AddLine([16.0,-0.375,0.0], [-16.0,-0.375,0.0])
   line3 = rs.AddLine([-16.0,-0.375,0.0], [-16.0,0.375,0.0])
   line4 = rs.AddLine([-16.0,0.375,0.0], [16.0,0.375,0.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

   line1 = rs.AddLine([0.375,16.0,0.0], [-0.375,16.0,0.0])
   line2 = rs.AddLine([-0.375,16.0,0.0], [-0.375,-16.0,0.0])
   line3 = rs.AddLine([-0.375,-16.0,0.0], [0.375,-16.0,0.0])
   line4 = rs.AddLine([0.375,-16.0,0.0], [0.375,16.0,0.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, 0.75])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])


def BuildWhiteboard():
   line1 = rs.AddLine([-12.0,-121.875,77.75], [-12.0,-49.875,77.75])
   line2 = rs.AddLine([-12.0,-49.875,77.75], [-12.0,-49.875,29.75])
   line3 = rs.AddLine([-12.0,-49.875,29.75], [-12.0,-121.875,29.75])
   line4 = rs.AddLine([-12.0,-121.875,29.75], [-12.0,-121.875,77.75])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([-12.0, 0.0, 0.0],[-11.875, 0.0, 0.0])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

   line1 = rs.AddLine([-12.0,-121.875,77.75], [-12.0,-49.875,77.75])
   line2 = rs.AddLine([-12.0,-49.875,77.75], [-12.0,-49.875,29.75])
   line3 = rs.AddLine([-12.0,-49.875,29.75], [-12.0,-121.875,29.75])
   line4 = rs.AddLine([-12.0,-121.875,29.75], [-12.0,-121.875,77.75])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([-12.0, 0.0, 0.0],[-11.875, 0.0, 0.0])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

def BuildDesk():

   line1 = rs.AddLine([0.0, 0.0, 30.0], [96.0, 0.0, 30.0])
   line2 = rs.AddLine([96.0, 0.0, 30.0], [96.0, -96.0, 30.0])
   line3 = rs.AddLine([96.0, -96.0, 30.0], [0.0, -96.0, 30.0])
   line4 = rs.AddLine([0.0, -96.0, 30.0], [0.0, 0.0, 30.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, -1.25])
   ext1 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext1)
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])




   line1 = rs.AddLine([22.0, -96.0, 30.0], [22.0, -54.0, 30.0])
   line2 = rs.AddLine([22.0, -54.0, 30.0], [96.0, -54.0, 30.0])
   line3 = rs.AddLine([96.0, -54.0, 30.0], [96.0, -96.0, 30.0])
   line4 = rs.AddLine([96.0, -96.0, 30.0], [22.0, -96.0, 30.0])
   curve1 = rs.JoinCurves([line1,line2,line3,line4])
   path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, 0.0, -1.25])
   ext2 = rs.ExtrudeCurve(curve1,path1)
   rs.CapPlanarHoles(ext2)
   rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])
   
   leftover1 = rs.BooleanDifference(ext1, ext2, True)
   leftover2 = rs.BooleanDifference(leftover1, rs.AddCylinder([41.0,-41.125,27.0], 4.0, 23.0), True)

   rs.AddCylinder([80.0,-38.0,28.25], 1.45, 16.0)


print("Building P2-141 Office...")

#BuildTopBottom()
#BuildWalls()
#BuildDoor(True)
BuildMullions()

#Warning:does not place properly just builds assets
#BuildBookShelf()
#BuildTable()
#BuildWhiteboard()

#BuildDesk() 71.875
#rs.AddCylinder([80.0,-38.0,0.0], 28.25, 2.0)
#line1 = rs.AddLine([0.0, 0.75, 0.0], [0.0, 0.75, 30])
#line2 = rs.AddLine([0.0, 0.75, 30], [96.0, 0.75, 30])
#line3 = rs.AddLine([96.0, 0.75, 30], [96.0, 0.75, 0.0])
#line4 = rs.AddLine([96.0, 0.75, 0.0], [0.0, 0.75, 0.0])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, -0.75, 0.0])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])
#rs.AddCylinder([80.0,-38.0,0.0], 30.0, 2.0)

#rs.AddCylinder([80.0,-38.0,0.0], 29.0, 2.0)

#line1 = rs.AddLine([6.75, 0.0, 71.875], [6.75, -14.125, 71.875])
#line2 = rs.AddLine([6.75, -14.125, 71.875], [6.75, -14.125, 29.375])
#line3 = rs.AddLine([6.75, -14.125, 29.375], [6.75, 0.0, 29.375])
#line4 = rs.AddLine([6.75, 0.0, 29.375], [6.75, 0.0, 71.875])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([6.75, 0.0, 0.0],[7.5, 0.0, 0.0])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

#line1 = rs.AddLine([95.25, 0.0, 71.875], [95.25, -14.125, 71.875])
#line2 = rs.AddLine([95.25, -14.125, 71.875], [95.25, -14.125, 29.375])
#line3 = rs.AddLine([95.25, -14.125, 29.375], [95.25, 0.0, 29.375])
#line4 = rs.AddLine([95.25, 0.0, 29.375], [95.25, 0.0, 71.875])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([95.25, 0.0, 0.0],[96.0, 0.0, 0.0])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])


#line1 = rs.AddLine([6.75, 0.75, 30.0], [6.75, 0.75, 71.875])
#line2 = rs.AddLine([6.75, 0.75, 71.875], [96.0, 0.75, 71.875])
#line3 = rs.AddLine([96.0, 0.75, 71.875], [96.0, 0.75, 30.0])
#line4 = rs.AddLine([96.0, 0.75, 30.0], [6.75, 0.75, 30.0])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0.0, 0.0, 0.0],[0.0, -0.75, 0.0])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

#line1 = rs.AddLine([7.5, 0.0, 71.875], [7.5, -14.0, 71.875])
#line2 = rs.AddLine([7.5, -14.0, 71.875], [7.5, -14.0, 51.375])
#line3 = rs.AddLine([7.5, -14.0, 51.375], [7.5, 0.0, 51.375])
#line4 = rs.AddLine([7.5, 0.0, 51.375], [7.5, 0.0, 71.875])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([7.5,0.0,  0.0],[95.25, 0.0, 0.0])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

#line1 = rs.AddLine([79.625, -38.0, 28.75], [80.375, -38.0, 28.75])
#line2 = rs.AddLine([80.375, -38.0, 28.75], [80.375, 0.0, 28.75])
#line3 = rs.AddLine([80.375, 0.0, 28.75], [79.625, 0.0, 28.75])
#line4 = rs.AddLine([79.625, 0.0, 28.75], [79.625, -38.0, 28.75])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0,0.0,  28.75],[0, 0.0, 16.75])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])


#line1 = rs.AddLine([0, 0,0 ], [0.75, 0,0 ])
#line2 = rs.AddLine([0.75, 0,0 ], [0.75, -95.25,0 ])
#line3 = rs.AddLine([0.75, -95.25,0], [0, -95.25,0])
#line4 = rs.AddLine([0, -95.25,0], [0, 0,0 ])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0,0.0,  0],[0, 0.0, 28.75])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

#line1 = rs.AddLine([0, -96.0,0 ], [0, -95.25,0 ])
#line2 = rs.AddLine([0, -95.25,0 ], [22, -95.25,0])
#line3 = rs.AddLine([22, -95.25,0], [22, -96,0])
#line4 = rs.AddLine([22, -96,0], [0, -96.0,0 ])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0,0.0,  0],[0, 0.0, 28.75])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

#line1 = rs.AddLine([0.75, -95.25,0 ], [0.75, -80,0 ])
#line2 = rs.AddLine([0.75, -80,0], [21.25, -80,0])
#line3 = rs.AddLine([21.25, -80,0], [21.25, -95.25,0])
#line4 = rs.AddLine([21.25, -95.25,0], [0.75, -95.25,0 ])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0,0.0,  0],[0, 0.0, 28.75])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])

#0,  -->14.3   14.375-->28.75
#line1 = rs.AddLine([21.25, -95.25,0 ], [22, -95.25,0 ])
#line2 = rs.AddLine([22, -95.25,0], [22, -80,0])
#line3 = rs.AddLine([22, -80,0], [21.25, -80,0])
#line4 = rs.AddLine([21.25, -80,0], [21.25, -95.25,0 ])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0,0.0,  0],[0, 0.0, 14.3])
#ext423 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext423)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])


#line1 = rs.AddLine([0, -45.0,0 ], [0, -44.25,0 ])
#line2 = rs.AddLine([0, -44.25,0 ], [10, -44.25,0])
#line3 = rs.AddLine([10, -44.25,0], [10, -45,0])
#line4 = rs.AddLine([10, -45,0], [0, -45.0,0 ])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0,0.0,  0],[0, 0.0, 28.75])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])




#14.625  --6
start=7.5+ 14.5*5.0 
#end=7.5+ 14.625*6.0
#line1 = rs.AddLine([start, -14.0, 71.875], [start, -14.0, 51.375])
#line2 = rs.AddLine([start, -14.0, 51.375], [end, -14.0, 51.375])
#line3 = rs.AddLine([end, -14.0, 51.375], [end, -14.0, 71.875])
#line4 = rs.AddLine([end, -14.0, 71.875], [start, -14.0, 71.875])
#curve1 = rs.JoinCurves([line1,line2,line3,line4])
#path1 = rs.AddLine([0,-14.0,  0.0],[0, -14.125, 0.0])
#ext42 = rs.ExtrudeCurve(curve1,path1)
#rs.CapPlanarHoles(ext42)
#rs.DeleteObjects([line1,line2,line3,line4, path1, curve1])