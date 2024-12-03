import re

f = open("../input.txt")
input = f.read()
x = re.findall(r"mul\(\d+,\d+\)|do\(\)|don't\(\)", input)
result = 0
do = True
print(x)
for match in x:
    if(match == "do()"):
        do = True
    elif(match == "don't()"):
        do = False
    elif(do == True):
        vals = match[4:-1].split(",")
        result += int(vals[0]) * int(vals[1])
print (result)
