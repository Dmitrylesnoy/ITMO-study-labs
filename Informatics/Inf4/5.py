import time

xml_str = open("schedule.xml").read().replace("\n", "")


def chechSpaceLine(line):
    return any(i != " " for i in line)


def convertingXML2JSON(contain, tab_count=0):
    obj = obje = ""
    descrip = ""
    flag_obj = False
    flag_founhead = False
    flag_depth = False
    i1 = 0
    i2 = len(contain)

    for i in range(len(contain)):
        l = contain[i]
        if l == "<":
            flag_obj = True
            continue
        if flag_obj:
            if l != ">":
                obj += l
            else:
                flag_obj = False
                i1 = i + 1
                break

    if obj == "":
        flag_depth = True

    while not flag_founhead and not flag_depth:
        obje = ""
        for i in range(i1, len(contain)):
            l = contain[i]
            if l == "<":
                flag_obj = True
                i2 = i
                continue
            if flag_obj:
                if l != ">":
                    obje += l
                else:
                    flag_obj = False
                    if obje[1:] == obj:
                        flag_founhead = True
                        break
                    else:
                        obje = ""

    descrip = contain[i1:i2]
    other = ""

    if not flag_depth:  # if object contains some more description
        conv, one_head = convertingXML2JSON(descrip, tab_count + 1)

        if not one_head:  # if object has more than 1 characteristic
            obj_c = str(
                "\n"
                + tab_count * "    "
                + '"'
                + obj
                + '": {'
                + conv[:-1]
                + "\n"
                + tab_count * "    "
                + "}"
            )
        else:  # if object has 1 characteristic
            obj_c = str("\n" + tab_count * "    " + '"' + obj + '": ' + conv)

        other = contain[i2 + 2 + len(obje) :]

        # if text contain more than one object in one hierarchy level
        otherc, one_head = convertingXML2JSON(other, tab_count)
        obj_c += "," + otherc

    else:  # if entered text is a final description text of object
        if not chechSpaceLine(descrip):
            obj_c = ""
        else:
            obj_c = str('"' + descrip + '"')

    return obj_c, flag_depth


def write_avro(data, filename):
    with open(filename, "wb") as f:
        f.write(b"Obj")  # AVRO magic header bytes
        schema = b'{"type": "record", "name": "root", "fields": [{"name": "description", "type": "string"}]}'
        schema_length = len(schema).to_bytes()
        f.write(schema_length)
        f.write(schema)

        # Write data with newlines for readability
        for item in data:
            desc = item.get("description", "")
            desc_bytes = desc.encode("utf-8")
            desc_length = len(desc_bytes).to_bytes()
            f.write(desc_length)
            f.write(desc_bytes)
            f.write(b"\n")  # Add newline for readability


timer = time.time()

json_data = convertingXML2JSON(xml_str)[0][:-1]
# Convert JSON data to a list for AVRO
avro_data = []
for line in json_data.splitlines():
    if line.strip():  # Ignore empty lines
        avro_data.append({"description": line.strip()})

write_avro(avro_data, "schedule.avro")

timer_ = time.time()
print((timer_ - timer) * 100)
