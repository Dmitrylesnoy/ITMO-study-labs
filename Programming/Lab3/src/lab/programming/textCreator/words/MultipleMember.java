package lab.programming.textCreator.words;

import java.util.ArrayList;

public class MultipleMember extends SentenceMember {
    ArrayList<SentenceMember> members;
    String knot = "";

    public MultipleMember(String knot) {
        this.knot = knot;
    }

    public MultipleMember(ArrayList<SentenceMember> members) {
        this.members = members;
    }

    void setKnot(String knot) {
        this.knot = knot;
    }

    @Override
    void addSubject(SentenceMember member) {
        members.add(member);
    }

    @Override
    public String toString() {
        String str = "";
        for (SentenceMember member : members) {
            str += member.toString() + ", " + knot;
        }
        return str;
    }

}
