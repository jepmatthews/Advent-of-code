f = open("../input.txt")

line = f.readline()

print(line)
memory = []

freeSpace = False
fileIndex = 0
for char in line:
    if(freeSpace):
        memory += (["."] * int(char))
        freeSpace = False
    else:
        memory += ([str(fileIndex)] * int(char))
        fileIndex += 1
        freeSpace = True

defragmentCursor = len(memory) - 1
freeMemoryCursor = 0
while(defragmentCursor >= 0 and defragmentCursor > freeMemoryCursor):
    if(memory[defragmentCursor] != "."):
        while(memory[freeMemoryCursor] != "." and defragmentCursor > freeMemoryCursor):
            freeMemoryCursor += 1
        if(defragmentCursor > freeMemoryCursor):
            memory[freeMemoryCursor] = memory[defragmentCursor]
            memory[defragmentCursor] = "."
    defragmentCursor -= 1
    # print(memory)
    # print()

result = 0

for index,val in enumerate(memory):
    if(val == "."):
        break
    print(f'{index}*{val}')
    result += index * int(val)

print (result)
# print("final state")
# print(memory)

