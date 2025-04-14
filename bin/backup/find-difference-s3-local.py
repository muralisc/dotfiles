
#  \find ~/data -type f -not -ipath "*/tmp/*" | sort > ~/local_file_list.txt
import argparse
import os

from pathlib import Path

parser = argparse.ArgumentParser(description='find diff')
parser.add_argument('s3_file_list', help='S3 file list')
parser.add_argument('--local-paths-file', default='', help='local path')

args = parser.parse_args()



s3_paths=[]
with open(args.s3_file_list) as file:
    s3_paths = file.read().splitlines();

only_in_local = []
in_both = 0
# with open(args.local_paths_file) as file:
#     while line := file.readline():
#         clean_line = line.rstrip()
#         no_home = clean_line.replace("/home/murali/", "")
#         if no_home not in s3_paths:
#             print(clean_line)
#             only_in_local.append(clean_line)
#         else:
#             # print(f"Found {clean_line}")
#             in_both += 1

# 2 =============================
all_files = Path("~/data").expanduser().glob("**/*")
all_files_no_dir = filter(lambda file_path: str(file_path).find('/tmp/') == -1 and file_path.is_dir() == False, all_files)
result = list(map(str, all_files_no_dir))
print(len(result))
for clean_line in result:
    # print(clean_line)
    no_home = clean_line.replace("/home/murali/", "")
    if no_home not in s3_paths:
        # print(clean_line)
        only_in_local.append(clean_line)
    else:
        # print(f"Found {clean_line}")
        in_both += 1

print(f"Files only in local {len(only_in_local)}")
print(f"Fles in both {in_both}")

only_in_local_bytes = 0
for local_file in only_in_local:
    only_in_local_bytes += os.path.getsize(local_file)
    # print(f"{local_file} = {only_in_local_bytes/(1024*1024*1024):.2f}")
print(f"bytes = {only_in_local_bytes}")
print(f"kb {only_in_local_bytes/1024:.2f}")
print(f"mb {only_in_local_bytes/(1024*1024):.2f}")
print(f"gb {only_in_local_bytes/(1024*1024*1024):.2f}")
