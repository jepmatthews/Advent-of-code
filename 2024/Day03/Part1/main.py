import re

f = open("../input.txt")
input = f.read()
x = re.findall(r'mul\(\d+,\d+\)', input)
result = 0
for mul in x:
    vals = mul[4:-1].split(",")
    result += int(vals[0]) * int(vals[1])
print (result)
