package lab.programming.textCreator;

import lab.programming.textCreator.sentences.BaseSentence;
import lab.programming.textCreator.words.MultipleMember;
import lab.programming.textCreator.words.Predicate;
import lab.programming.textCreator.words.Speed;
import lab.programming.textCreator.words.Subject;
import lab.programming.textCreator.words.SubjectBuilder;

public class Story {
    public static void main(String[] args) {

        Subject car = new Subject("автомобиль", "красивый");
        Speed speedTest = Speed.SLOW;

        Predicate predicate1 = new Predicate("выехал", "аккуратно", Speed.FAST);
        predicate1.setPlace(new Subject("заросли", "маковые"), "из");
        predicate1.addSubject(new SubjectBuilder(car));

        Subject travelers = new Subject("путешественники", "наши");
        Predicate predicate2 = new Predicate("увидали");
        Subject strangeMachine = new Subject("машина", "странная");

        predicate2.addSubject(new SubjectBuilder(travelers));
        predicate2.setPlace(new Subject("дорога", "недалекая"), "от");
        predicate2.setDefenition(strangeMachine.toString());

        Predicate carDescrip1 = new Predicate("напоминавшую", "", Speed.NOMOVE);
        carDescrip1.addSubject(new SubjectBuilder(strangeMachine));
        Subject strMachDescrip1 = new Subject("чистка", "механическая");
        Subject strMachDescrip2 = new Subject("трактор", "");

        MultipleMember multyMember1 = new MultipleMember("и");
        multyMember1.addSubject(predicate1);
        multyMember1.addSubject(predicate2);
        MultipleMember multyMember2 = new MultipleMember("не то");
        multyMember2.addSubject(carDescrip1);
        multyMember2.addSubject(new SubjectBuilder(strMachDescrip1));
        multyMember2.addSubject(new SubjectBuilder(strMachDescrip2));

        BaseSentence sent1 = new BaseSentence.SentenceBuider().addMultiple(multyMember1).addMultiple(multyMember2)
                .build();

        // writeln(sent1.toString());
        Predicate predicate3 = new Predicate("выехал", "аккуратно", Speed.FAST);
        predicate3.setPlace(new Subject("заросли", "маковые"), "из");
        predicate3.addSubject(new SubjectBuilder(car));

        writeln(Integer.toString(predicate1.hashCode()));
        writeln(Integer.toString(predicate3.hashCode()));
        if (predicate1.equals(predicate3))
            writeln("True");
        else
            writeln("False");

        SubjectBuilder buil = new SubjectBuilder(car);
    }

    private static void write(String s) {
        System.out.print(s);
    }

    private static void writeln(String s) {
        System.out.println(s);
    }
}
