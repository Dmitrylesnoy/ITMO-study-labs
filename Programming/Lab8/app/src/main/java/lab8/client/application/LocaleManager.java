package lab8.client.application;

import java.util.Locale;
import java.util.ResourceBundle;

public class LocaleManager {
    private static LocaleManager instance;
    private ResourceBundle resourceBundle;
    private Locale currentLocale;

    // Поддерживаемые языки
    public static final Locale ENGLISH = new Locale("en", "US");
    public static final Locale RUSSIAN = new Locale("ru", "RU");
    public static final Locale FRENCH = new Locale("fr", "FR");

    private LocaleManager() {
        // По умолчанию используем системную локаль
        currentLocale = Locale.getDefault();
        resourceBundle = ResourceBundle.getBundle("lab8.shared.i18n.messages", currentLocale);
    }

    public static synchronized LocaleManager getInstance() {
        if (instance == null) {
            instance = new LocaleManager();
        }
        return instance;
    }

    public void setLocale(Locale locale) {
        currentLocale = locale;
        resourceBundle = ResourceBundle.getBundle("lab8.shared.i18n.messages", currentLocale);
    }

    public Locale getCurrentLocale() {
        return currentLocale;
    }

    public String getString(String key) {
        return resourceBundle.getString(key);
    }

    public ResourceBundle getResourceBundle() {
        return resourceBundle;
    }
}