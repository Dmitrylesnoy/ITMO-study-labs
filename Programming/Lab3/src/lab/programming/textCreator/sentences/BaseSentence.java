package lab.programming.textCreator.sentences;

import java.util.ArrayList;

import lab.programming.textCreator.words.*;

public class BaseSentence {
    private ArrayList<SentenceMember> members = new ArrayList<SentenceMember>();

    public BaseSentence() {
    }

    public BaseSentence(SentenceBuider buider) {
        this.members = buider.members;
    }

    public void addMember(SentenceMember member) {
        members.add(member);
    }

    @Override
    public String toString() {
        String str = "";
        for (int i = 0; i < members.size() - 1; i++) {
            str += members.get(i).toString() + ", ";
        }
        str += members.get(members.size() - 1).toString() + ". ";
        return str;
    }

    @Override
    public int hashCode() {
        int code = 0;
        if (members != null) {
            for (SentenceMember member : members) {
                code = code * 31 + member.hashCode();
            }
        }
        return code;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || getClass() != obj.getClass())
            return false;

        BaseSentence objs = (BaseSentence) obj;

        if (!members.equals(objs.members))
            return false;

        return true;
    }

    public static class SentenceBuider {
        private ArrayList<SentenceMember> members = new ArrayList<SentenceMember>();

        public SentenceBuider addPredicate(Predicate pred) {
            members.add(pred);
            return this;
        }

        public SentenceBuider addSBuider(Subject sub) {
            members.add(new SubjectBuilder(sub).build());
            return this;
        }

        public SentenceBuider addMultiple(MultipleMember mult) {
            members.add(mult);
            return this;
        }

        public BaseSentence build() {
            BaseSentence sent = new BaseSentence(this);
            return sent;
        }
    }
}
