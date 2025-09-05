import time

xml_str = open("schedule.xml").read().replace("\n", "")


def chechSpaceLine(line):
    for i in line:
        if i != " ":
            return True
    return False


def convertingXML2JSON(contain, tab_count=0):
    """
    Convert object contain from XML to JSON
    """
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

        if not one_head:  # if object has more than 1 charachteristics
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

        # if text contain more than one object in one ierachiah level
        otherc, one_head = convertingXML2JSON(other, tab_count)
        obj_c += "," + otherc

    else:  # if entered text is a final description text of object
        if not chechSpaceLine(descrip):
            obj_c = ""
        else:
            obj_c = str('"' + descrip + '"')

    return obj_c, flag_depth


timer = time.time()
with open("schedule.json", "w") as f:
    f.write("{" + convertingXML2JSON(xml_str)[0][:-1] + "\n}")
    timer_ = time.time()
    print((timer_ - timer) * 1000)
