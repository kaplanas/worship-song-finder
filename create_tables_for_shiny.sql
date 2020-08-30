USE wsf_shiny;

-- Table of songs
DROP TABLE IF EXISTS songs;
CREATE TABLE songs AS
(SELECT SongID,
        SongName
 FROM wsf.songs);
COMMIT;

-- Table of song instances
DROP TABLE IF EXISTS songinstances;
CREATE TABLE songinstances AS
(SELECT songinstances.SongInstanceID,
        SongInstance, songinstances.ArrangementID,
        SongID,
        LastLyricsYear,
        LyricsCopyright,
        LastTuneYear,
        TuneCopyright,
        ArrangementCopyright
 FROM wsf.songinstances 
      LEFT JOIN (SELECT SongInstanceID,
                        MAX(CopyrightYear) AS LastLyricsYear,
                        GROUP_CONCAT(CONCAT('(C) ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS LyricsCopyright
                 FROM wsf.songinstances_lyrics
                      JOIN (SELECT lyrics.LyricsID,
                                   CopyrightYear,
                                   GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                          THEN CopyrightHolderName
                                                END
                                                ORDER BY CopyrightHolderName
                                                SEPARATOR ', ') AS CopyrightHolderNames
                            FROM wsf.lyrics
                                 JOIN wsf.lyrics_copyrightholders
                                      ON lyrics.LyricsID = lyrics_copyrightholders.LyricsID
                                 JOIN wsf.copyrightholders
                                      ON lyrics_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                            GROUP BY lyrics.LyricsID,
                                     CopyrightYear) lyrics
                           ON songinstances_lyrics.LyricsID = lyrics.LyricsID
                 GROUP BY SongInstanceID) lyrics
      ON songinstances.SongInstanceID = lyrics.SongInstanceID
      LEFT JOIN (SELECT SongInstanceID,
                        MAX(CopyrightYear) AS LastTuneYear,
                        GROUP_CONCAT(CONCAT('(C) ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS TuneCopyright
                 FROM wsf.songinstances_tunes
                      JOIN (SELECT tunes.TuneID,
                                   CopyrightYear,
                                   GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                          THEN CopyrightHolderName
                                                END
                                                ORDER BY CopyrightHolderName
                                                SEPARATOR ', ') AS CopyrightHolderNames
                            FROM wsf.tunes
                                 JOIN wsf.tunes_copyrightholders
                                      ON tunes.TuneID = tunes_copyrightholders.TuneID
                                 JOIN wsf.copyrightholders
                                      ON tunes_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                            GROUP BY tunes.TuneID,
                                     CopyrightYear) tunes
                           ON songinstances_tunes.TuneID = tunes.TuneID
                 GROUP BY SongInstanceID) tunes
      ON songinstances.SongInstanceID = tunes.SongInstanceID
      LEFT JOIN (SELECT ArrangementID,
                        GROUP_CONCAT(CONCAT('(C) ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS ArrangementCopyright
                 FROM (SELECT arrangements.ArrangementID,
                              CopyrightYear,
                              GROUP_CONCAT(CASE WHEN copyrightholders.CopyrightHolderID <> 1
                                                     THEN CopyrightHolderName
                                           END
                                           ORDER BY CopyrightHolderName
                                           SEPARATOR ', ') AS CopyrightHolderNames
                       FROM wsf.arrangements
                            JOIN wsf.arrangements_copyrightholders
                                 ON arrangements.ArrangementID = arrangements_copyrightholders.ArrangementID
                            JOIN wsf.copyrightholders
                                 ON arrangements_copyrightholders.CopyrightHolderID = copyrightholders.CopyrightHolderID
                       GROUP BY arrangements.ArrangementID,
                                CopyrightYear) a
                 GROUP BY ArrangementID) arrangements
      ON songinstances.ArrangementID = arrangements.ArrangementID);
COMMIT;

-- Table of artists
DROP TABLE IF EXISTS artists;
CREATE TABLE artists AS
(SELECT ArtistID,
        LastName,
        FirstName,
        GenderName
 FROM wsf.artists
      LEFT JOIN wsf.genders
      ON artists.GenderID = genders.GenderID);
COMMIT;

-- Table that connects song instances and artists
DROP TABLE IF EXISTS songinstances_artists;
CREATE TABLE songinstances_artists AS
(SELECT DISTINCT songinstances.SongInstanceID,
        SongID, ArtistID,
        'lyricist' AS Role
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_artists
      ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID
 UNION ALL
 SELECT DISTINCT songinstances.SongInstanceID,
        SongID, ArtistID,
        'composer' AS Role
 FROM wsf.songinstances
      JOIN wsf.songinstances_tunes
      ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
      JOIN wsf.tunes_artists
      ON songinstances_tunes.TuneID = tunes_artists.TuneID
 UNION ALL
 SELECT DISTINCT songinstances.SongInstanceID,
        SongID, ArtistID, 'arranger' AS Role
 FROM wsf.songinstances
      JOIN wsf.arrangements_artists
      ON songinstances.ArrangementID = arrangements_artists.ArrangementID);
COMMIT;

-- Table of topics
DROP TABLE IF EXISTS topics;
CREATE TABLE topics AS
(SELECT TopicID,
        TopicName
 FROM wsf.topics);
COMMIT;

-- Table that connects songs and topics
DROP TABLE IF EXISTS songs_topics;
CREATE TABLE songs_topics AS
(SELECT SongID, TopicID
 FROM wsf.songs_topics);
COMMIT;

-- Table of books of the Bible
DROP TABLE IF EXISTS bible_books;
CREATE TABLE bible_books AS
(SELECT BookID,
        BookName,
        BookAbbreviation
 FROM wsf.booksofthebible);
COMMIT;

-- Table of scripture references
DROP TABLE IF EXISTS scripturereferences;
CREATE TABLE scripturereferences AS
(SELECT ScriptureReferenceID,
        booksofthebible.BookID,
        BookName,
        BookAbbreviation,
        Chapter,
        Verse
 FROM wsf.scripturereferences
      JOIN wsf.booksofthebible
      ON scripturereferences.BookID = booksofthebible.BookID);
COMMIT;

-- Table that connects song instances and scripture references
DROP TABLE IF EXISTS songinstances_scripturereferences;
CREATE TABLE songinstances_scripturereferences AS
(SELECT DISTINCT songinstances.SongInstanceID, SongID, ScriptureReferenceID
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_scripturereferences
      ON songinstances_lyrics.LyricsID = lyrics_scripturereferences.LyricsID);
COMMIT;

-- Table of languages
DROP TABLE IF EXISTS languages;
CREATE TABLE languages AS
(SELECT LanguageID,
        LanguageName
 FROM wsf.languages);
COMMIT;

-- Table that connects song instances and languages
DROP TABLE IF EXISTS songinstances_languages;
CREATE TABLE songinstances_languages AS
(SELECT DISTINCT songinstances.SongInstanceID,
        SongID, LanguageID
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID);
COMMIT;

-- Table of songbooks
DROP TABLE IF EXISTS songbooks;
CREATE TABLE songbooks AS
(SELECT SongbookID,
        SongbookName,
        SongbookAbbreviation,
        IncludeInSearch
 FROM wsf.songbooks);
COMMIT;

-- Table that conects song instances and songbooks
DROP TABLE IF EXISTS songinstances_songbooks;
CREATE TABLE songinstances_songbooks AS
(SELECT songinstances.SongInstanceID,
        SongID,
        songbooks.SongbookID,
        SongbookName,
        SongbookAbbreviation,
        IncludeInSearch,
        songbookvolumes.SongbookVolumeID,
        SongbookVolume,
        EntryNumber
 FROM wsf.songinstances
      JOIN wsf.songbookentries
      ON songinstances.SongInstanceID = songbookentries.SongInstanceID
      JOIN wsf.songbooks
      ON songbookentries.SongbookID = songbooks.SongbookID
      LEFT JOIN wsf.songbookvolumes
      ON songbookentries.SongbookVolumeID = songbookvolumes.SongbookVolumeID);
COMMIT;


-- Table of arrangement types
DROP TABLE IF EXISTS arrangementtypes;
CREATE TABLE arrangementtypes AS
(SELECT ArrangementTypeID,
        ArrangementType
 FROM wsf.arrangementtypes);
COMMIT;

-- Table that connects song instances and arrangement types
DROP TABLE IF EXISTS songinstances_arrangementtypes;
CREATE TABLE songinstances_arrangementtypes AS
(SELECT DISTINCT SongInstanceID,
        SongID,
        ArrangementTypeID
 FROM wsf.songinstances
      JOIN wsf.arrangements_arrangementtypes
      ON songinstances.ArrangementID = arrangements_arrangementtypes.ArrangementID);
COMMIT;

-- Table of key signatures
DROP TABLE IF EXISTS keysignatures;
CREATE TABLE keysignatures AS
(SELECT KeySignatureID,
        PitchName,
        accidentals.AccidentalID,
        AccidentalSymbol,
        modes.ModeID,
        ModeName
 FROM wsf.keysignatures
      JOIN wsf.pitches
      ON keysignatures.PitchID = pitches.PitchID
      JOIN wsf.accidentals
      ON keysignatures.AccidentalID = accidentals.AccidentalID
      JOIN wsf.modes
      ON keysignatures.ModeID = modes.ModeID);
COMMIT;

-- Table that connects song instances and key signatures
DROP TABLE IF EXISTS songinstances_keysignatures;
CREATE TABLE songinstances_keysignatures AS
(SELECT songinstances.SongInstanceID,
        SongID, KeySignatureID
 FROM wsf.songinstances
      JOIN wsf.songinstances_keysignatures
      ON songinstances.SongInstanceID = songinstances_keysignatures.SongInstanceID);
COMMIT;

-- Table of time signatures
DROP TABLE IF EXISTS timesignatures;
CREATE TABLE timesignatures AS
(SELECT TimeSignatureID,
        TimeSignatureBeat,
        TimeSignatureMeasure
 FROM wsf.timesignatures);
COMMIT;

-- Table that connects song instances and time signatures
DROP TABLE IF EXISTS songinstances_timesignatures;
CREATE TABLE songinstances_timesignatures AS
(SELECT songinstances.SongInstanceID,
        SongID,
        TimeSignatureID
 FROM wsf.songinstances
      JOIN wsf.songinstances_timesignatures
      ON songinstances.SongInstanceID = songinstances_timesignatures.SongInstanceID);
COMMIT;

-- Table of meters
DROP TABLE IF EXISTS meters;
CREATE TABLE meters AS
(SELECT MeterID,
        Meter,
        Multiplier
 FROM wsf.meters);
COMMIT;

-- Table that connects song instances and meters
DROP TABLE IF EXISTS songinstances_meters;
CREATE TABLE songinstances_meters AS
(SELECT songinstances.SongInstanceID,
        SongID,
        MeterID
 FROM wsf.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_meters
      ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
 UNION DISTINCT
 SELECT songinstances.SongInstanceID, SongID, MeterID
 FROM wsf.songinstances
      JOIN wsf.songinstances_tunes
      ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
      JOIN wsf.tunes_meters
      ON songinstances_tunes.TuneID = tunes_meters.TuneID);
COMMIT;

-- Table of lyrics first lines for all song instances
DROP TABLE IF EXISTS lyrics_first_lines;
CREATE TABLE lyrics_first_lines AS
(SELECT SongInstanceID,
        FirstLine,
        RefrainFirstLine
 FROM wsf.songinstances_lyrics
      JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID);
COMMIT;

-- Table of worship slots
DROP TABLE IF EXISTS worshipslots;
CREATE TABLE worshipslots AS
(SELECT WorshipSlotID,
        WorshipSlot,
        WorshipSlotOrder
 FROM wsf.worshipslots);
COMMIT;

-- Table that connects song instances and lyrics
DROP TABLE IF EXISTS songinstances_lyrics;
CREATE TABLE songinstances_lyrics AS
(SELECT SongInstanceID,
        LyricsID
 FROM wsf.songinstances_lyrics);
COMMIT;

-- Table that connects lyrics and translations
DROP TABLE IF EXISTS lyrics_translations;
CREATE TABLE lyrics_translations AS
(SELECT lyrics.LyricsID,
        TranslatedFromID,
        CASE WHEN lyrics.LanguageID = sources.LanguageID
                  THEN 'alt'
             ELSE 'tr'
        END AS Type
 FROM wsf.lyrics
      JOIN wsf.lyrics_translations
      ON lyrics.LyricsID = lyrics_translations.LyricsID
      JOIN wsf.lyrics sources
      ON lyrics_translations.TranslatedFromID = sources.LyricsID);
COMMIT;

-- Table that connects lyrics and artists
DROP TABLE IF EXISTS lyrics_artists;
CREATE TABLE lyrics_artists AS
(SELECT LyricsID, ArtistID
 FROM wsf.lyrics_artists);
COMMIT;

-- Table of worship history
DROP TABLE IF EXISTS worshiphistory;
CREATE TABLE worshiphistory AS
(SELECT WorshipHistoryID,
        SongInstanceID,
        WorshipHistoryDate,
        WorshipSlotID
 FROM wsf.worshiphistory);
COMMIT;