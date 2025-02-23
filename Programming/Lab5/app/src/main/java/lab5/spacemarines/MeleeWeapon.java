package lab5.spacemarines;

import jakarta.xml.bind.annotation.XmlEnum;
import jakarta.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlEnum(String.class)
public enum MeleeWeapon {
    CHAIN_SWORD,
    POWER_SWORD,
    POWER_BLADE,
    POWER_FIST;
}
