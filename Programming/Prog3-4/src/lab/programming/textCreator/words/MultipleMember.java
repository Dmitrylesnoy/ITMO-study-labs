package lab.programming.textCreator.words;

import java.util.ArrayList;

import lab.programming.textCreator.exceptions.NonAviableMethod;

public class MultipleMember extends SentenceMember {
    private ArrayList<SentenceMember> members = new ArrayList<SentenceMember>();
    private String knot = "";

    public MultipleMember() {
    }

    public MultipleMember(String knot) {
        this.knot = knot;
    }

    public MultipleMember(ArrayList<SentenceMember> members) {
        this.members = members;
    }

    public void setKnot(String knot) {
        this.knot = knot;
    }

    public void addSubject(SentenceMember member) {
        members.add(member);
    }

    @Override
    public String toString() {
        String str = "";
        for (int i = 0; i < members.size() - 1; i++) {
            str += members.get(i).toString() + ", " + knot + " ";
        }
        str += members.get(members.size()-1).toString();
        return str;
    }

    @Override
    public int hashCode() {
        int code = 0;
        if (members != null) {
            for (SentenceMember member : members) {
                code = code * 31 + member.hashCode();
            }
            code = code * 31 + knot.hashCode();
        }
        return code;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;

        MultipleMember objs = (MultipleMember) obj;

        if (!knot.equals(objs.knot))
            return false;
        if (!members.equals(objs.members))
            return false;

        return true;
    }
}
