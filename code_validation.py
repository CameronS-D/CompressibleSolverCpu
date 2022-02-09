import os

print("Compiling...")
os.system("gfortran -O3 -g 2D_compressible.f90")
print("Compile complete. Executing...")
os.system("a.exe")
print("\nExecuted program. Checking output validity...\n")

# with open("original_output/vort0001") as original:
#     with open("vort0001") as new:
#         for line in original.readlines():
#             if line.strip() == "":
#                 break

#             ori_x, ori_y, ori_vort = line.split()
#             new_x, new_y, new_vort = new.readline().split()

#             if (float(ori_x) != float(new_x) or float(ori_y) != float(new_y) or
#                 float(ori_vort) != float(new_vort)):
#                 print("FAILURE! Output is incorrect.")
#                 print(ori_x, new_x)
#                 print(ori_y, new_y)
#                 print(ori_vort, new_vort)
#                 quit()

# print("SUCCESS! Output is correct.")
