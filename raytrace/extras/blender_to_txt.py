# usage: blender your_file.blend -P blender_to_txt.py
# This will produce a 'cube.txt' file in the current working directory.
import bpy
outp=open(r'cube.txt','w')
all=bpy.data.objects
for obj in all:
    for face in obj.data.polygons:
        for v in face.vertices:
            for crd in obj.data.vertices[v].co:
                outp.write(str(crd)+'\n')
        
        color=obj.material_slots[face.material_index].material.diffuse_color
        for comp in color:
            outp.write(str(int(255*comp))+'\n')
