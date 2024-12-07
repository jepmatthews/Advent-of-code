class Rule:
    def __init__(self,first,second):
        self.first = first
        self.second = second
    def __str__(self):
        return f"{self.first}|{self.second}"
    def match(self, pages):
        if((self.first not in pages) or (self.second not in pages)):
            return True

        return pages[self.first] < pages [self.second]



f = open("../input.txt")

lines = f.readlines()
lines = list(map(lambda str: str.replace("\n",""),lines))
emptyLineIndex = lines.index("")
ruleLines = lines[0:emptyLineIndex]
pageLines = lines[emptyLineIndex+1:]

rules = []
for line in ruleLines:
    parts = line.split("|")
    rules.append(Rule(int(parts[0]),int(parts[1])))

updates = []
for line in pageLines:
    update = {}
    pages = line.split(",")
    for (index,page) in enumerate(pages):
        update[int(page)] = index
    update["middlePage"] = int(pages[int(len(pages)/2)]) 
    updates.append(update)

result = 0

for update in updates:
    safe = True
    for rule in rules:
        if(not rule.match(update)):
            safe = False
            break
    if safe:
        result += update["middlePage"]

print(result)