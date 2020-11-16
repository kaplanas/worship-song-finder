USE wsf_shiny;
SET group_concat_max_len = 150000;

-- SONGBOOK ENTRY DATA --

-- Table of songbooks
DROP TABLE IF EXISTS wsf_shiny.songbooks;
CREATE TABLE wsf_shiny.songbooks AS
(SELECT SongbookID,
        SongbookName,
        SongbookAbbreviation,
        IncludeInSearch
 FROM wsf.songbooks);
COMMIT;

-- SONG INSTANCE DATA --

-- Table that conects song instances and songbooks
DROP TABLE IF EXISTS wsf_shiny.songinstances_songbooks;
CREATE TABLE wsf_shiny.songinstances_songbooks AS
(SELECT SongInstanceID, SongID, SongbookID, SongbookName,
        SongbookAbbreviation, IncludeInSearch, SongbookVolumeID,
        SongbookVolume, EntryNumber,
        CONCAT(SongbookName,
               CASE WHEN SongbookVolume IS NULL THEN ''
                    WHEN SongbookID IN (0, 6) THEN ''
                    ELSE ' '
               END,
               EntryStringNoName) AS EntryString,
        CASE WHEN SongbookID = 12
                  THEN CONCAT(EntryStringNoName, EntryNumber)
             ELSE EntryStringNoName
        END AS EntryStringNoName
 FROM (SELECT songinstances.SongInstanceID, SongID,
              songbooks.SongbookID, SongbookName,
              SongbookAbbreviation, IncludeInSearch,
              songbookvolumes.SongbookVolumeID,
              SongbookVolume, EntryNumber,
              CONCAT(CASE WHEN SongbookVolume IS NULL THEN ''
                          WHEN songbooks.SongbookID IN (0, 6) THEN ''
                          ELSE CONCAT('(', SongbookVolume, ')')
                     END,
                     CASE WHEN songbooks.SongbookID = 12 THEN ''
                          WHEN EntryNumber IS NULL THEN ''
                          WHEN EntryNumber = '' THEN ''
                          ELSE CONCAT(' ', EntryNumber)
                     END) AS EntryStringNoName
       FROM wsf.songinstances
            JOIN wsf.songbookentries
            ON songinstances.SongInstanceID = songbookentries.SongInstanceID
            JOIN wsf.songbooks
            ON songbookentries.SongbookID = songbooks.SongbookID
            LEFT JOIN wsf.songbookvolumes
            ON songbookentries.SongbookVolumeID = songbookvolumes.SongbookVolumeID) s);
COMMIT;

-- Table of song instances
DROP TABLE IF EXISTS wsf_shiny.songinstances;
CREATE TABLE wsf_shiny.songinstances AS
(SELECT songinstances.SongInstanceID,
        SongInstance, LOWER(SongInstance) AS SongInstanceLower,
        songinstances.ArrangementID,
        SongID,
        LastLyricsYear,
        LyricsCopyright,
        LastTuneYear,
        TuneCopyright,
        ArrangementCopyright,
        ArrangementTypes,
        SongbookEntries,
        NumEntries,
        PrettyScriptureList AS ScriptureReferences,
        COALESCE(IncludeInSearch, 'N') AS IncludeInSearch
 FROM wsf.songinstances 
      LEFT JOIN (SELECT SongInstanceID,
                        MAX(CopyrightYear) AS LastLyricsYear,
                        GROUP_CONCAT(DISTINCT CONCAT('© ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS LyricsCopyright
                 FROM wsf.songinstances_lyrics
                      JOIN (SELECT lyrics.LyricsID,
                                   CopyrightYear,
                                   GROUP_CONCAT(DISTINCT
                                                CASE WHEN copyrightholders.CopyrightHolderID <> 1
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
                        GROUP_CONCAT(DISTINCT CONCAT('© ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS TuneCopyright
                 FROM wsf.songinstances_tunes
                      JOIN (SELECT tunes.TuneID,
                                   CopyrightYear,
                                   GROUP_CONCAT(DISTINCT
                                                CASE WHEN copyrightholders.CopyrightHolderID <> 1
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
      LEFT JOIN (SELECT a.ArrangementID,
                        GROUP_CONCAT(DISTINCT CONCAT('© ', CopyrightYear, ' ', CopyrightHolderNames)
                                     ORDER BY CopyrightYear
                                     SEPARATOR '; ') AS ArrangementCopyright,
                        GROUP_CONCAT(ArrangementType
                                     ORDER BY ArrangementType
                                     SEPARATOR ', ') AS ArrangementTypes
                 FROM (SELECT arrangements.ArrangementID,
                              CopyrightYear,
                              GROUP_CONCAT(DISTINCT
                                           CASE WHEN copyrightholders.CopyrightHolderID <> 1
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
                      LEFT JOIN wsf.arrangements_arrangementtypes
                      ON a.ArrangementId = arrangements_arrangementtypes.ArrangementID
                      LEFT JOIN wsf.arrangementtypes
                      ON arrangements_arrangementtypes.ArrangementTypeID = arrangementtypes.ArrangementTypeID
                 GROUP BY a.ArrangementID) arrangements
      ON songinstances.ArrangementID = arrangements.ArrangementID
      LEFT JOIN (SELECT SongInstanceID,
                        GROUP_CONCAT(DISTINCT EntryString
                                     ORDER BY SongbookName, EntryNumber
                                     SEPARATOR ', ') AS SongbookEntries,
                        COUNT(*) AS NumEntries
                 FROM wsf_shiny.songinstances_songbooks
                 GROUP BY SongInstanceID) songbook_entries
      ON songinstances.SongInstanceID = songbook_entries.SongInstanceID
      LEFT JOIN (SELECT SongInstanceID,
                        MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songinstances_songbooks
                 GROUP BY SongInstanceID) include_songinstance
      ON songinstances.SongInstanceID = include_songinstance.SongInstanceID
      LEFT JOIN wsf.prettyscripturelists
      ON songinstances.SongInstanceID = prettyscripturelists.SongInstanceID);
COMMIT;

-- Table of artists
DROP TABLE IF EXISTS wsf_shiny.artists;
CREATE TABLE wsf_shiny.artists AS
(SELECT artists.ArtistID,
        LastName,
        FirstName,
        CONCAT(CASE WHEN FirstName IS NULL THEN ''
                    ELSE CONCAT(FirstName, ' ')
               END,
               LastName) AS ArtistName,
        GenderName
 FROM wsf.artists
      LEFT JOIN wsf.genders
      ON artists.GenderID = genders.GenderID);
COMMIT;

-- Table that connects song instances and artists
DROP TABLE IF EXISTS wsf_shiny.songinstances_artists;
CREATE TABLE wsf_shiny.songinstances_artists
(SongInstanceID int,
 SongID int,
 ArtistID int,
 Role varchar(50),
 IncludeInSearch varchar(1));
INSERT INTO wsf_shiny.songinstances_artists
(SELECT DISTINCT songinstances.SongInstanceID, SongID, ArtistID,
        'lyricist' AS Role, IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_artists
      ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID);
INSERT INTO wsf_shiny.songinstances_artists
(SELECT DISTINCT songinstances.SongInstanceID, SongID, ArtistID,
        'composer' AS Role, IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_tunes
      ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
      JOIN wsf.tunes_artists
      ON songinstances_tunes.TuneID = tunes_artists.TuneID);
INSERT INTO wsf_shiny.songinstances_artists
(SELECT DISTINCT songinstances.SongInstanceID, SongID, ArtistID,
        'arranger' AS Role, IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.arrangements_artists
      ON songinstances.ArrangementID = arrangements_artists.ArrangementID);
COMMIT;

-- For each set of lyrics, construct a pretty artist string
-- Also, add lyricists to table of song instances and artists
DROP PROCEDURE IF EXISTS getTranslators;
DELIMITER $
CREATE PROCEDURE getTranslators()
BEGIN
    
    DECLARE numTranslations int;
    
    DROP TABLE IF EXISTS wsf_shiny.lyrics_artists;
    CREATE TABLE wsf_shiny.lyrics_artists AS
    (SELECT lyrics_artists.LyricsID,
        	GROUP_CONCAT(DISTINCT artists.ArtistName
       	                 ORDER BY artists.LastName, artists.FirstName
       	                 SEPARATOR ', ') AS OriginalPrettyArtistString,
        	GROUP_CONCAT(DISTINCT artists.ArtistName
       	                 ORDER BY artists.LastName, artists.FirstName
       	                 SEPARATOR ', ') AS PrettyArtistString,
            TranslatedFromID
     FROM wsf.lyrics_artists
          INNER JOIN wsf_shiny.artists
          ON lyrics_artists.ArtistID = artists.ArtistID
          LEFT JOIN wsf.lyrics_translations
          ON lyrics_artists.LyricsID = lyrics_translations.LyricsID
     GROUP BY lyrics_artists.LyricsID);
     
     SELECT COUNT(*)
     INTO numTranslations
     FROM wsf_shiny.lyrics_artists
     WHERE TranslatedFromID IS NOT NULL;
     
     WHILE numTranslations > 0 DO
         
         INSERT INTO wsf_shiny.songinstances_artists
         (SELECT DISTINCT songinstances.SongInstanceID, SongID,
                 la.ArtistID, 'lyricist' AS Role, IncludeInSearch
          FROM wsf_shiny.songinstances
               INNER JOIN wsf.songinstances_lyrics
               ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
               INNER JOIN wsf_shiny.lyrics_artists
               ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID
               INNER JOIN wsf_shiny.lyrics_artists translations
               ON lyrics_artists.TranslatedFromID = translations.LyricsID
               INNER JOIN wsf.lyrics_artists la
               ON translations.LyricsID = la.LyricsID);
         
         UPDATE wsf_shiny.lyrics_artists
                LEFT JOIN wsf.lyrics l1
                ON lyrics_artists.LyricsID = l1.LyricsID
                LEFT JOIN wsf_shiny.lyrics_artists la2
                ON lyrics_artists.TranslatedFromID = la2.LyricsID
                LEFT JOIN wsf.lyrics l2
                ON la2.LyricsID = l2.LyricsID
         SET lyrics_artists.PrettyArtistString = CONCAT(la2.OriginalPrettyArtistString,
                                                        CASE WHEN l1.LanguageID = l2.LanguageID
                                                                  THEN ', alt. '
                                                             ELSE ', tr. '
                                                        END,
                                                        lyrics_artists.PrettyArtistString),
             lyrics_artists.TranslatedFromID = la2.TranslatedFromID
         WHERE lyrics_artists.TranslatedFromID IS NOT NULL;
         
         SELECT COUNT(*)
         INTO numTranslations
         FROM wsf_shiny.lyrics_artists
         WHERE TranslatedFromID IS NOT NULL;
         
     END WHILE;
     
     ALTER TABLE wsf_shiny.lyrics_artists
     DROP OriginalPrettyArtistString,
     DROP TranslatedFromID;
     
END$
DELIMITER ;
SET SQL_SAFE_UPDATES = 0;
CALL getTranslators();
SET SQL_SAFE_UPDATES = 1;
COMMIT;

-- Update table of artists
ALTER TABLE wsf_shiny.artists
ADD IncludeInSearch varchar(1);
SET SQL_SAFE_UPDATES = 0;
UPDATE wsf_shiny.artists
       LEFT JOIN (SELECT ArtistID,
                         MAX(IncludeInSearch) AS IncludeInSearch
                  FROM wsf_shiny.songinstances_artists
                  GROUP BY ArtistID) include_artist
       ON artists.ArtistID = include_artist.ArtistID
SET artists.IncludeInSearch = include_artist.IncludeInSearch;
SET SQL_SAFE_UPDATES = 1;
COMMIT;

-- Table that connects song instances and scripture references
DROP TABLE IF EXISTS wsf_shiny.songinstances_scripturereferences;
CREATE TABLE wsf_shiny.songinstances_scripturereferences AS
(SELECT DISTINCT songinstances.SongInstanceID, SongID,
        ScriptureReferenceID, IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_scripturereferences
      ON songinstances_lyrics.LyricsID = lyrics_scripturereferences.LyricsID);
COMMIT;

-- Table of books of the Bible
DROP TABLE IF EXISTS wsf_shiny.bible_books;
CREATE TABLE wsf_shiny.bible_books AS
(SELECT BookID,
        BookName,
        BookAbbreviation
 FROM wsf.booksofthebible);
COMMIT;

-- Table of scripture references
DROP TABLE IF EXISTS wsf_shiny.scripturereferences;
CREATE TABLE wsf_shiny.scripturereferences AS
(SELECT scripturereferences.ScriptureReferenceID,
        booksofthebible.BookID,
        BookName,
        BookAbbreviation,
        Chapter,
        Verse,
        IncludeInSearch
 FROM wsf.scripturereferences
      JOIN wsf.booksofthebible
      ON scripturereferences.BookID = booksofthebible.BookID
      LEFT JOIN (SELECT ScriptureReferenceID,
                        MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songinstances_scripturereferences
                 GROUP BY ScriptureReferenceID) include_scripturereference
      ON scripturereferences.ScriptureReferenceID = include_scripturereference.ScriptureReferenceID);
COMMIT;

-- Table that connects song instances and languages
DROP TABLE IF EXISTS wsf_shiny.songinstances_languages;
CREATE TABLE wsf_shiny.songinstances_languages AS
(SELECT DISTINCT songinstances.SongInstanceID,
        SongID, LanguageID, IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID);
COMMIT;

-- Table of languages
DROP TABLE IF EXISTS wsf_shiny.languages;
CREATE TABLE wsf_shiny.languages AS
(SELECT languages.LanguageID,
        LanguageName,
        IncludeInSearch
 FROM wsf.languages
      LEFT JOIN (SELECT LanguageID,
                        MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songinstances_languages
                 GROUP BY LanguageID) include_language
      ON languages.LanguageID = include_language.LanguageID);
COMMIT;

-- Table that connects song instances and arrangement types
DROP TABLE IF EXISTS wsf_shiny.songinstances_arrangementtypes;
CREATE TABLE wsf_shiny.songinstances_arrangementtypes AS
(SELECT DISTINCT SongInstanceID,
        SongID,
        ArrangementTypeID,
        IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.arrangements_arrangementtypes
      ON songinstances.ArrangementID = arrangements_arrangementtypes.ArrangementID);
COMMIT;

-- Table of arrangement types
DROP TABLE IF EXISTS wsf_shiny.arrangementtypes;
CREATE TABLE wsf_shiny.arrangementtypes AS
(SELECT arrangementtypes.ArrangementTypeID,
        ArrangementType,
        IncludeInSearch
 FROM wsf.arrangementtypes
      LEFT JOIN (SELECT ArrangementTypeID,
                        MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songinstances_arrangementtypes
                 GROUP BY ArrangementTypeID) include_arrangementtype
      ON arrangementtypes.ArrangementTypeID = include_arrangementtype.ArrangementTypeID);
COMMIT;

-- Table that connects song instances and key signatures
DROP TABLE IF EXISTS wsf_shiny.songinstances_keysignatures;
CREATE TABLE wsf_shiny.songinstances_keysignatures AS
(SELECT songinstances.SongInstanceID,
        SongID, KeySignatureID, IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_keysignatures
      ON songinstances.SongInstanceID = songinstances_keysignatures.SongInstanceID);
COMMIT;

-- Table of key signatures
DROP TABLE IF EXISTS wsf_shiny.keysignatures;
CREATE TABLE wsf_shiny.keysignatures AS
(SELECT keysignatures.KeySignatureID,
        PitchName,
        accidentals.AccidentalID,
        AccidentalSymbol,
        modes.ModeID,
        ModeName,
        CONCAT(PitchName,
               CASE WHEN accidentals.AccidentalID = 3 THEN ''
                    ELSE AccidentalSymbol
               END,
               CASE WHEN modes.ModeID = 1 THEN ''
                    WHEN modes.ModeID = 2 THEN 'm'
                    ELSE CONCAT(' ', ModeName)
               END) AS KeySignatureString,
        IncludeInSearch
 FROM wsf.keysignatures
      JOIN wsf.pitches
      ON keysignatures.PitchID = pitches.PitchID
      JOIN wsf.accidentals
      ON keysignatures.AccidentalID = accidentals.AccidentalID
      JOIN wsf.modes
      ON keysignatures.ModeID = modes.ModeID
      LEFT JOIN (SELECT KeySignatureID,
                        MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songinstances_keysignatures
                 GROUP BY KeySignatureID) include_keysignature
      ON keysignatures.KeySignatureID = include_keysignature.KeySignatureID);
COMMIT;

-- Table that connects song instances and time signatures
DROP TABLE IF EXISTS wsf_shiny.songinstances_timesignatures;
CREATE TABLE wsf_shiny.songinstances_timesignatures AS
(SELECT songinstances.SongInstanceID,
        SongID,
        TimeSignatureID,
        IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_timesignatures
      ON songinstances.SongInstanceID = songinstances_timesignatures.SongInstanceID);
COMMIT;

-- Table of time signatures
DROP TABLE IF EXISTS wsf_shiny.timesignatures;
CREATE TABLE wsf_shiny.timesignatures AS
(SELECT timesignatures.TimeSignatureID,
        TimeSignatureBeat,
        TimeSignatureMeasure,
        CONCAT(TimeSignatureBeat, '/',
               TimeSignatureMeasure) AS TimeSignatureString,
        IncludeInSearch
 FROM wsf.timesignatures
      LEFT JOIN (SELECT TimeSignatureID,
                        MAX(IncludeInSearch) IncludeInSearch
                 FROM wsf_shiny.songinstances_timesignatures
                 GROUP BY TimeSignatureID) include_timesignature
      ON timesignatures.TimeSignatureID = include_timesignature.TimeSignatureID);
COMMIT;

-- Table that connects song instances and meters
DROP TABLE IF EXISTS wsf_shiny.songinstances_meters;
CREATE TABLE wsf_shiny.songinstances_meters AS
(SELECT songinstances.SongInstanceID, SongID, MeterID,IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      JOIN wsf.lyrics_meters
      ON songinstances_lyrics.LyricsID = lyrics_meters.LyricsID
 UNION DISTINCT
 SELECT songinstances.SongInstanceID, SongID, MeterID, IncludeInSearch
 FROM wsf_shiny.songinstances
      JOIN wsf.songinstances_tunes
      ON songinstances.SongInstanceID = songinstances_tunes.SongInstanceID
      JOIN wsf.tunes_meters
      ON songinstances_tunes.TuneID = tunes_meters.TuneID);
COMMIT;

-- Table of meters
DROP TABLE IF EXISTS wsf_shiny.meters;
CREATE TABLE wsf_shiny.meters AS
(SELECT meters.MeterID,
        Meter,
        Multiplier,
        CONCAT(Meter,
               CASE WHEN Multiplier IS NULL THEN ''
                    ELSE CONCAT(' ', Multiplier)
               END) AS MeterString,
        IncludeInSearch
 FROM wsf.meters
      LEFT JOIN (SELECT MeterID,
                        MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songinstances_meters
                 GROUP BY MeterID) include_meter
      ON meters.MeterID = include_meter.MeterID);
COMMIT;

-- Table of lyrics first lines for all song instances
DROP TABLE IF EXISTS wsf_shiny.lyrics_first_lines;
CREATE TABLE wsf_shiny.lyrics_first_lines AS
(SELECT songinstances.SongInstanceID,
        FirstLine,
        1 AS FirstLineOrder,
        lyrics.LyricsID,
        IncludeInSearch
 FROM wsf_shiny.songinstances
      INNER JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      INNER JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID
 UNION ALL
 SELECT songinstances.SongInstanceID,
        RefrainFirstLine AS FirstLine,
        2 AS FirstLineOrder,
        lyrics.LyricsID,
        IncludeInSearch
 FROM wsf_shiny.songinstances
      INNER JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      INNER JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID
 WHERE lyrics.RefrainFirstLine IS NOT NULL);
COMMIT;

-- Add more song instance info
ALTER TABLE wsf_shiny.songinstances
ADD Lyricists varchar(1000),
ADD Composers varchar(1000),
ADD Arrangers varchar(1000),
ADD KeySignatures varchar(1000),
ADD TimeSignatures varchar(1000),
ADD HTML blob;
SET SQL_SAFE_UPDATES = 0;
UPDATE wsf_shiny.songinstances
       LEFT JOIN (SELECT SongInstanceID,
                         GROUP_CONCAT(DISTINCT PrettyArtistString
                                      SEPARATOR ', ') AS Lyricists
                  FROM wsf.songinstances_lyrics
                       INNER JOIN wsf_shiny.lyrics_artists
                       ON songinstances_lyrics.LyricsID = lyrics_artists.LyricsID
                  GROUP BY SongInstanceID) lyricists
       ON songinstances.SongInstanceID = lyricists.SongInstanceID
       LEFT JOIN (SELECT SongInstanceID,
                         GROUP_CONCAT(DISTINCT ArtistName
                                      ORDER BY LastName, FirstName
                                      SEPARATOR ', ') AS Composers
                  FROM wsf_shiny.songinstances_artists
                       INNER JOIN wsf_shiny.artists
                       ON songinstances_artists.ArtistID = artists.ArtistID
                  WHERE Role = 'composer'
                  GROUP BY SongInstanceID) composers
       ON songinstances.SongInstanceID = composers.SongInstanceID
       LEFT JOIN (SELECT SongInstanceID,
                         GROUP_CONCAT(DISTINCT ArtistName
                                      ORDER BY LastName, FirstName
                                      SEPARATOR ', ') AS Arrangers
                  FROM wsf_shiny.songinstances_artists
                       INNER JOIN wsf_shiny.artists
                       ON songinstances_artists.ArtistID = artists.ArtistID
                  WHERE Role = 'arranger'
                  GROUP BY SongInstanceID) arrangers
       ON songinstances.SongInstanceID = arrangers.SongInstanceID
       LEFT JOIN (SELECT SongInstanceID,
                         GROUP_CONCAT(DISTINCT KeySignatureString
                                      ORDER BY PitchName, AccidentalID, ModeID
                                      SEPARATOR ', ') AS KeySignatures
                  FROM wsf.songinstances_keysignatures
                       INNER JOIN wsf_shiny.keysignatures
                       ON songinstances_keysignatures.KeySignatureID = keysignatures.KeySignatureID
                  GROUP BY SongInstanceID) keysignatures
       ON songinstances.SongInstanceID = keysignatures.SongInstanceID
       LEFT JOIN (SELECT SongInstanceID,
                         GROUP_CONCAT(DISTINCT TimeSignatureString
                                      ORDER BY TimeSignatureMeasure, TimeSignatureBeat
                                      SEPARATOR ', ') AS TimeSignatures
                  FROM wsf.songinstances_timesignatures
                       INNER JOIN wsf_shiny.timesignatures
                       ON songinstances_timesignatures.TimeSignatureID = timesignatures.TimeSignatureID
                  GROUP BY SongInstanceID) timesignatures
       ON songinstances.SongInstanceID = timesignatures.SongInstanceID
       LEFT JOIN (SELECT SongInstanceID,
                         GROUP_CONCAT(CONCAT('<i>', FirstLine, '</i>')
                                      ORDER BY FirstLineOrder, LyricsID
                                      SEPARATOR '<br/>') AS LyricsFirstLines
                  FROM wsf_shiny.lyrics_first_lines
                  GROUP BY SongInstanceID) firstlines
       ON songinstances.SongInstanceID = firstlines.SongInstanceID
SET songinstances.Lyricists = lyricists.Lyricists,
    songinstances.Composers = composers.Composers,
    songinstances.Arrangers = arrangers.Arrangers,
    songinstances.KeySignatures = keysignatures.KeySignatures,
    songinstances.TimeSignatures = timesignatures.TimeSignatures,
    songinstances.HTML = CONCAT('<hr/> <h3>', songinstances.SongInstance, '</h3>',
                                CASE WHEN songinstances.SongbookEntries IS NULL THEN ''
                                     ELSE CONCAT('<p>', songinstances.SongbookEntries, '</p>')
                                END,
                                CASE WHEN songinstances.ArrangementTypes IS NULL THEN ''
                                     ELSE CONCAT('<p>', songinstances.ArrangementTypes, '</p>')
                                END,
                                CASE WHEN keysignatures.KeySignatures IS NOT NULL
                                          OR timesignatures.TimeSignatures IS NOT NULL
                                          THEN CONCAT('<p>',
                                                      COALESCE(keysignatures.KeySignatures, ''),
                                                      CASE WHEN keysignatures.KeySignatures IS NOT NULL
                                                                AND timesignatures.TimeSignatures IS NOT NULL
                                                                THEN '; '
                                                           ELSE ''
                                                      END,
                                                      COALESCE(timesignatures.TimeSignatures, ''),
                                                      '</p>')
                                     ELSE ''
                                END,
                                CASE WHEN songinstances.ScriptureReferences IS NULL THEN ''
                                     ELSE CONCAT('<p>', songinstances.ScriptureReferences, '</p>')
                                END,
                                CASE WHEN lyricists.Lyricists = composers.Composers
                                          THEN CONCAT('<p>Lyrics & Music:&nbsp;', lyricists.Lyricists, '</p>')
                                     ELSE CONCAT(CASE WHEN lyricists.Lyricists IS NULL THEN ''
                                                      ELSE CONCAT('<p>Lyrics:&nbsp;', lyricists.Lyricists, '</p>')
                                                 END,
                                                 CASE WHEN composers.Composers IS NULL THEN ''
                                                      ELSE CONCAT('<p>Music:&nbsp;', composers.Composers, '</p>')
                                                 END)
                                END,
                                CASE WHEN arrangers.Arrangers IS NULL THEN ''
                                     ELSE CONCAT('<p>Arr.:&nbsp;', arrangers.Arrangers, '</p>')
                                END,
                                CASE WHEN songinstances.LyricsCopyright = songinstances.TuneCopyright
                                          THEN CONCAT('<p>', songinstances.LyricsCopyright, '</p>')
                                     ELSE CONCAT(CASE WHEN songinstances.LyricsCopyright IS NULL THEN ''
                                                      ELSE CONCAT('<p>Lyrics ', songinstances.LyricsCopyright, '</p>')
                                                 END,
                                                 CASE WHEN songinstances.TuneCopyright IS NULL THEN ''
                                                      ELSE CONCAT('<p>Tune ', songinstances.TuneCopyright, '</p>')
                                                 END)
                                END,
                                CASE WHEN songinstances.ArrangementCopyright IS NULL THEN ''
                                     ELSE CONCAT('<p>Arrangement ', songinstances.ArrangementCopyright, '</p>')
                                END,
                                CONCAT('<p>', LyricsFirstLines, '</p>'));
SET SQL_SAFE_UPDATES = 1;
COMMIT;

-- SONG DATA --

-- Table of songs
DROP TABLE IF EXISTS wsf_shiny.songs;
CREATE TABLE wsf_shiny.songs
(SongID int,
 SongName varchar(500),
 SongNameLower varchar(500),
 SongNameSort varchar(500),
 PanelName varchar(10000),
 Copyrighted varchar(1),
 SongbookEntries varchar(1000),
 IncludeInSearch varchar(1));
INSERT INTO wsf_shiny.songs
(SELECT songs.SongID,
        SongName, LOWER(SongName) AS SongNameLower,
        REGEXP_REPLACE(SongName, '^[\'\"¡¿]', '') AS SongNameSort,
        CONCAT(SongName,
               CASE WHEN songinstances.OtherTitles IS NOT NULL
                         THEN CONCAT('<br/>', songinstances.OtherTitles)
                    ELSE ''
               END) AS PanelName,
        CASE WHEN copyrighted_songs.SongID IS NULL THEN 'N'
             ELSE 'Y'
        END AS Copyrighted,
        SongbookEntries,
        COALESCE(IncludeInSearch, 'N') AS IncludeInSearch
 FROM wsf.songs
      LEFT JOIN (SELECT songs.SongID,
                        GROUP_CONCAT(DISTINCT CONCAT('<i>', SongInstance, '</i>')
                                     ORDER BY SongInstance
                                     SEPARATOR '<br/>') AS OtherTitles
                 FROM wsf.songinstances
                      INNER JOIN wsf.songs
                      ON songinstances.SongID = songs.SongID
                 WHERE LOWER(SongInstance) <> LOWER(songs.SongName)
                 GROUP BY songs.SongID) songinstances
      ON songs.SongID = songinstances.SongID
      LEFT JOIN (SELECT DISTINCT songinstances.SongID
                 FROM wsf.songinstances
                      INNER JOIN wsf.songinstances_lyrics
                      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
                      INNER JOIN wsf.lyrics_copyrightholders
                      ON songinstances_lyrics.LyricsID = lyrics_copyrightholders.LyricsID
                 WHERE lyrics_copyrightholders.CopyrightHolderID <> 1) copyrighted_songs
      ON songs.SongID = copyrighted_songs.SongID
      LEFT JOIN (SELECT SongID,
                        GROUP_CONCAT(DISTINCT EntryString
                                     ORDER BY SongbookName, EntryNumber
                                     SEPARATOR ', ') AS SongbookEntries
                 FROM wsf_shiny.songinstances_songbooks
                 GROUP BY SongID) songbook_entries
      ON songs.SongID = songbook_entries.SongID
      LEFT JOIN (SELECT SongID, MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songinstances_songbooks
                 GROUP BY SongID) include_song
      ON songs.SongID = include_song.SongID);
COMMIT;

-- Table that connects songs and topics
DROP TABLE IF EXISTS wsf_shiny.songs_topics;
CREATE TABLE wsf_shiny.songs_topics AS
(SELECT songs.SongID, TopicID, IncludeInSearch
 FROM wsf_shiny.songs
      INNER JOIN wsf.songs_topics
      ON songs.SongID = songs_topics.SongID);
COMMIT;

-- Table of topics
DROP TABLE IF EXISTS wsf_shiny.topics;
CREATE TABLE wsf_shiny.topics AS
(SELECT topics.TopicID,
        TopicName,
        IncludeInSearch
 FROM wsf.topics
      LEFT JOIN (SELECT TopicID,
                        MAX(IncludeInSearch) AS IncludeInSearch
                 FROM wsf_shiny.songs_topics
                 GROUP BY TopicID) include_topic
      ON topics.TopicID = include_topic.TopicID);
COMMIT;

-- Update table of songs
ALTER TABLE wsf_shiny.songs
ADD Topics varchar(1000);
SET SQL_SAFE_UPDATES = 0;
UPDATE wsf_shiny.songs
       LEFT JOIN (SELECT SongID,
                         GROUP_CONCAT(TopicName
                                      ORDER BY TopicName
                                      SEPARATOR ', ') Topics
                  FROM wsf_shiny.songs_topics
                       INNER JOIN wsf.topics
                       ON songs_topics.SongID = topics.TopicID
                  GROUP BY SongID) topics
       ON songs.SongID = topics.SongID
SET songs.Topics = topics.Topics;
SET SQL_SAFE_UPDATES = 1;
COMMIT;

-- SONGBOOK OVERLAP DATA --

-- Table of shared songs.
DROP TABLE IF EXISTS wsf_shiny.songbook_overlap;
CREATE TABLE wsf_shiny.songbook_overlap AS
(SELECT DISTINCT SongbookID1, SongbookName1, SongbookID2,
        SongbookName2, si1.SongID,
        CASE WHEN IncludeInSearch1 = 'Y' AND IncludeInSearch2 = 'Y'
                  THEN 'Y'
             ELSE 'N'
        END AS IncludeInSearch
 FROM ((SELECT SongbookID AS SongbookID1,
               SongbookName AS SongbookName1,
               IncludeInSearch AS IncludeInSearch1
        FROM wsf.songbooks) s1
       CROSS JOIN (SELECT SongbookID AS SongbookID2,
                          SongbookName AS SongbookName2,
                          IncludeInSearch AS IncludeInSearch2
                   FROM wsf.songbooks) s2)
      INNER JOIN (SELECT SongbookID, SongID
                  FROM wsf_shiny.songinstances_songbooks) si1
      ON SongbookID1 = si1.SongbookID
      INNER JOIN (SELECT SongbookID, SongID
                  FROM wsf_shiny.songinstances_songbooks) si2
      ON SongbookID2 = si2.SongbookID
         AND si1.SongID = si2.SongID
 WHERE SongbookID1 < SongbookID2);
COMMIT;

-- WORSHIP HISTORY DATA --

-- Table of worship slots
DROP TABLE IF EXISTS wsf_shiny.worshipslots;
CREATE TABLE wsf_shiny.worshipslots AS
(SELECT WorshipSlotID,
        WorshipSlot,
        WorshipSlotOrder
 FROM wsf.worshipslots);
COMMIT;

-- Table of worship history
DROP TABLE IF EXISTS wsf_shiny.worshiphistory;
CREATE TABLE wsf_shiny.worshiphistory AS
(SELECT WorshipHistoryID,
        SongInstanceID,
        WorshipHistoryDate,
        WorshipSlotID
 FROM wsf.worshiphistory);
COMMIT;

-- PSALM SONG DATA --

-- Table of metrical psalms
DROP TABLE IF EXISTS wsf_shiny.metricalpsalms;
CREATE TABLE wsf_shiny.metricalpsalms AS
(SELECT metricalpsalms.MetricalPsalmID,
        metricalpsalms.PsalmNumber,
        metricalpsalms_prettyscripturelists.PrettyScriptureList
 FROM wsf.metricalpsalms
      LEFT JOIN wsf.metricalpsalms_prettyscripturelists
      ON metricalpsalms.MetricalPsalmID = metricalpsalms_prettyscripturelists.MetricalPsalmID);
COMMIT;

-- Table that connects psalm songs and lyrics
DROP TABLE IF EXISTS wsf_shiny.psalmsongs_lyrics;
CREATE TABLE wsf_shiny.psalmsongs_lyrics
(PsalmSongID varchar(10),
 LyricsID int,
 FirstLine varchar(1000),
 LanguageID int,
 PublicDomain varchar(1000),
 LyricsOrder int,
 IncludeInSearch varchar(1));
INSERT INTO wsf_shiny.psalmsongs_lyrics
(SELECT DISTINCT CONCAT('PS', psalmsongs.PsalmSongID) AS PsalmSongID,
        lyrics.LyricsID, lyrics.FirstLine, lyrics.LanguageID,
        public_domain.PublicDomain,
        ROW_NUMBER() OVER (PARTITION BY psalmsongs.PsalmSongID,
                                        songinstances.SongInstanceID
                           ORDER BY lyrics.LyricsID) AS LyricsOrder,
        IncludeInSearch
 FROM wsf.psalmsongs
      INNER JOIN wsf.songinstances
      ON psalmsongs.SongID = songinstances.SongID
      INNER JOIN wsf.songinstances_lyrics
      ON songinstances.SongInstanceID = songinstances_lyrics.SongInstanceID
      INNER JOIN wsf.lyrics
      ON songinstances_lyrics.LyricsID = lyrics.LyricsID
      INNER JOIN (SELECT LyricsID,
                         CASE WHEN MIN(CopyrightHolderID) = 1
                                   THEN 'Y'
                              ELSE 'N'
                         END AS PublicDomain
                  FROM wsf.lyrics_copyrightholders
                  GROUP BY LyricsID) public_domain
      ON songinstances_lyrics.LyricsID = public_domain.LyricsID
      INNER JOIN wsf_shiny.songs
      ON psalmsongs.SongID = songs.SongID);
INSERT INTO wsf_shiny.psalmsongs_lyrics
(SELECT CONCAT('MP', metricalpsalms.MetricalPsalmID) AS PsalmSongID,
		lyrics.LyricsID, lyrics.FirstLine, lyrics.LanguageID,
        public_domain.PublicDomain,
        ROW_NUMBER() OVER (PARTITION BY metricalpsalms.MetricalPsalmID
                           ORDER BY AvgVerse) AS LyricsOrder,
        'Y' AS IncludeInSearch
 FROM wsf.metricalpsalms
      INNER JOIN wsf.metricalpsalms_lyrics
      ON metricalpsalms.MetricalPsalmID = metricalpsalms_lyrics.MetricalPsalmID
      INNER JOIN wsf.lyrics
      ON metricalpsalms_lyrics.LyricsID = lyrics.LyricsID
      INNER JOIN (SELECT LyricsID,
                         CASE WHEN MIN(CopyrightHolderID) = 1
                                   THEN 'Y'
                              ELSE 'N'
                         END AS PublicDomain
                  FROM wsf.lyrics_copyrightholders
                  GROUP BY LyricsID) public_domain
      ON lyrics.LyricsID = public_domain.LyricsID
      LEFT JOIN (SELECT lyrics_scripturereferences.LyricsID,
                        AVG(Verse) AS AvgVerse
                 FROM wsf.lyrics_scripturereferences
                      INNER JOIN wsf.scripturereferences
                      ON lyrics_scripturereferences.ScriptureReferenceID = scripturereferences.ScriptureReferenceID
                 GROUP BY LyricsID) avg_verse
      ON metricalpsalms_lyrics.LyricsID = avg_verse.LyricsID
 WHERE metricalpsalms_lyrics.LyricsID NOT IN
       (SELECT LyricsID
        FROM wsf.songinstances_lyrics
             INNER JOIN wsf.songinstances
             ON songinstances_lyrics.SongInstanceID = songinstances.SongInstanceID
             INNER JOIN wsf.psalmsongs
             ON songinstances.SongID = psalmsongs.SongID));
COMMIT;

-- Table of lyrics tabs for psalm songs
DROP TABLE IF EXISTS wsf_shiny.psalmsongs_lyrics_tabs;
CREATE TABLE wsf_shiny.psalmsongs_lyrics_tabs AS
(SELECT PsalmSongID,
        CONCAT('Lyrics',
               CASE WHEN languages.LanguageID = 1 THEN ''
                    ELSE CONCAT(' (', LanguageName, ')')
               END) AS TabName,
        GROUP_CONCAT(FullLyrics
                     ORDER BY LyricsOrder
                     SEPARATOR ' ') AS FullLyrics
 FROM wsf_shiny.psalmsongs_lyrics
      INNER JOIN wsf.languages
      ON psalmsongs_lyrics.LanguageID = languages.LanguageID
      INNER JOIN wsf.all_lyrics
      ON psalmsongs_lyrics.LyricsID = all_lyrics.LyricsID
 WHERE PublicDomain = 'Y'
 GROUP BY PsalmSongID, 2);
COMMIT;

-- Table of psalm songs
DROP TABLE IF EXISTS wsf_shiny.psalmsongs;
CREATE TABLE wsf_shiny.psalmsongs
(PsalmSongID varchar(50),
 PsalmNumber int,
 SongID int,
 MetricalPsalmID int,
 SongOrMetricalPsalmID varchar(20),
 PsalmSongTypeID int,
 PsalmSongType varchar(100),
 PsalmSongTitle varchar(500),
 PanelName varchar(10000),
 PrettyScriptureList varchar(500),
 Artists varchar(500),
 IncludeInSearch varchar(1),
 HTMLInfo blob);
INSERT INTO wsf_shiny.psalmsongs
(SELECT CONCAT('PS', psalmsongs.PsalmSongID) AS PsalmSongID,
        PsalmNumber, psalmsongs.SongID, NULL AS MetricalPsalmID,
        CONCAT('ps', psalmsongs.SongID) AS SongOrMetricalPsalmID,
        psalmsongs.PsalmSongTypeID, PsalmSongType,
        SongName AS PsalmSongTitle, songs.PanelName,
		REGEXP_REPLACE(PrettyScriptureList, '^Ps [0-9]+:', '') AS PrettyScriptureList,
        Artists, IncludeInSearch,
        CONCAT('<br/><h3>', PsalmSongType, '</h3>',
               CASE WHEN songs.SongbookEntries IS NOT NULL
                         THEN CONCAT('<p>', songs.SongbookEntries, '</p>')
               END,
               '<p><b>Verses:</b> ',
               REGEXP_REPLACE(PrettyScriptureList, '^Ps [0-9]+:', ''),
               '</p>',
               CASE WHEN songinstances.Artists IS NOT NULL
                         THEN CONCAT('<p>', songinstances.Artists, '</p>')
               END,
               '<p>', firstlines.FirstLines, '</p>') AS HTMLInfo
 FROM wsf.psalmsongs
      LEFT JOIN wsf.psalmsongtypes
      ON psalmsongs.PsalmSongTypeID = psalmsongtypes.PsalmSongTypeID
      LEFT JOIN wsf_shiny.songs
      ON psalmsongs.SongID = songs.SongID
      LEFT JOIN wsf.psalmsongs_prettyscripturelists
      ON psalmsongs.PsalmSongID = psalmsongs_prettyscripturelists.PsalmSongID
      LEFT JOIN (SELECT SongID,
                        CASE WHEN Lyricists = Composers
                                  THEN CONCAT('Lyrics & Music: ', Lyricists)
                             ELSE CONCAT(CASE WHEN Lyricists IS NULL THEN ''
                                              ELSE CONCAT('Lyrics: ', Lyricists)
                                         END,
                                         CASE WHEN Lyricists IS NULL
                                                   OR Composers IS NULL
                                                   THEN ''
                                              ELSE '; '
                                         END,
                                         CASE WHEN Composers IS NULL THEN ''
                                              ELSE CONCAT('Music: ', Composers)
                                         END)
                        END AS Artists,
                        ROW_NUMBER() OVER (PARTITION BY SongID
                                           ORDER BY GREATEST(COALESCE(LastLyricsYear, 0),
                                                             COALESCE(LastTuneYear, 0))) AS RowNum
                 FROM wsf_shiny.songinstances) songinstances
      ON psalmsongs.SongID = songinstances.SongID
         AND songinstances.RowNum = 1
      LEFT JOIN (SELECT PsalmSongID,
                        GROUP_CONCAT(FirstLine
                                     ORDER BY LyricsOrder
                                     SEPARATOR '<br/>') AS FirstLines
                 FROM wsf_shiny.psalmsongs_lyrics
                 GROUP BY PsalmSongID) firstlines
      ON CONCAT('PS', psalmsongs.PsalmSongID) = firstlines.PsalmSongID);
INSERT INTO wsf_shiny.psalmsongs
(SELECT CONCAT('MP', metricalpsalms.MetricalPsalmID) AS PsalmSongID,
        PsalmNumber, NULL AS SongID, metricalpsalms.MetricalPsalmID,
        CONCAT('mp', metricalpsalms.MetricalPsalmID) AS SongOrMetricalPsalmID,
        NULL AS PsalmSongTypeID, 'Paraphrase' AS PsalmSongType,
        FirstLine AS PsalmSongTitle,
        CONCAT(FirstLine,
               CASE WHEN laterfirstlines.LaterFirstLines IS NULL
                         THEN ''
                    ELSE CONCAT('<br/>', laterfirstlines.LaterFirstLines)
               END) AS PanelName,
        PrettyScriptureList, Artists,
        'Y' AS IncludeInSearch,
        CONCAT('<br/><h3>Paraphrase</h3>',
               '<p><b>Verses:</b> ',
               REGEXP_REPLACE(PrettyScriptureList, '^Ps [0-9]+:', ''),
               '</p>',
               CASE WHEN artists.Artists IS NOT NULL
                         THEN CONCAT('<p>', artists.Artists, '</p>')
               END,
               '<p>', firstlines.FirstLines, '</p>') AS HTMLInfo
 FROM wsf.metricalpsalms
      INNER JOIN (SELECT PsalmSongID, FirstLine
                  FROM wsf_shiny.psalmsongs_lyrics
                  WHERE LyricsOrder = 1) not_psalmsongs
      ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = not_psalmsongs.PsalmSongID
	  LEFT JOIN wsf.metricalpsalms_prettyscripturelists
      ON metricalpsalms.MetricalPsalmID = metricalpsalms_prettyscripturelists.MetricalPsalmID
      LEFT JOIN (SELECT MetricalPsalmID,
                        CONCAT('Lyrics: ',
                               GROUP_CONCAT(DISTINCT ArtistName
                                            ORDER BY LastName, FirstName
                                            SEPARATOR ', ')) AS Artists
                 FROM wsf.metricalpsalms_lyrics
                      INNER JOIN wsf.lyrics_artists
                      ON metricalpsalms_lyrics.LyricsID = lyrics_artists.LyricsID
                      INNER JOIN wsf_shiny.artists
                      ON lyrics_artists.ArtistID = artists.ArtistID
                 GROUP BY MetricalPsalmID) artists
      ON metricalpsalms.MetricalPsalmID = artists.MetricalPsalmID
      LEFT JOIN (SELECT PsalmSongID,
                        GROUP_CONCAT(FirstLine
                                     ORDER BY LyricsOrder
                                     SEPARATOR '<br/>') AS FirstLines
                 FROM wsf_shiny.psalmsongs_lyrics
                 GROUP BY PsalmSongID) firstlines
      ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = firstlines.PsalmSongID
      LEFT JOIN (SELECT PsalmSongID,
                        GROUP_CONCAT(CONCAT('<i>', FirstLine, '</i>')
                                     ORDER BY LyricsOrder
                                     SEPARATOR '<br/>') AS LaterFirstLines
                 FROM wsf_shiny.psalmsongs_lyrics
                 WHERE LyricsOrder > 1
                 GROUP BY PsalmSongID) laterfirstlines
      ON CONCAT('MP', metricalpsalms.MetricalPsalmID) = laterfirstlines.PsalmSongID);
ALTER TABLE wsf_shiny.psalmsongs
CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
COMMIT;

-- Table of psalm song types
DROP TABLE IF EXISTS wsf_shiny.psalmsongtypes;
CREATE TABLE wsf_shiny.psalmsongtypes AS
(SELECT PsalmSongTypeID, PsalmSongType
 FROM wsf.psalmsongtypes);
COMMIT;

-- Table that connects psalm songs and alternative tunes
DROP TABLE IF EXISTS wsf_shiny.psalmsongs_alternativetunes;
CREATE TABLE wsf_shiny.psalmsongs_alternativetunes
(PsalmSongID varchar(10),
 TuneID int,
 TuneDisplayName varchar(100),
 Notes varchar(1000),
 IncludeInSearch varchar(1));
INSERT INTO wsf_shiny.psalmsongs_alternativetunes
(SELECT psalmsongs.PsalmSongID,
        tunes.TuneID,
        CONCAT(CASE WHEN tunes.RealTuneName = 1
                         THEN CONCAT(TuneName, ' (')
                    ELSE ''
               END,
               COALESCE(tunes.CanonicalSongName,
                        canonical_song.SongName),
               CASE WHEN tunes.RealTuneName = 1
                         THEN ')'
                    ELSE ''
               END) AS TuneDisplayName,
        REPLACE(REPLACE(Notes, '#T#',
                        CONCAT('"',
                               COALESCE(tunes.CanonicalSongName,
                                        canonical_song.SongName),
                               '"')),
                '#L#', CONCAT('"', PsalmSongTitle, '"')) AS Notes,
        psalmsongs.IncludeInSearch
 FROM wsf_shiny.psalmsongs
      INNER JOIN wsf.alternativetunes
      ON psalmsongs.SongOrMetricalPsalmID = alternativetunes.SongOrMetricalPsalmID
      INNER JOIN wsf.tunes
      ON alternativetunes.TuneID = tunes.TuneID
      INNER JOIN wsf.tunes_copyrightholders
      ON tunes.TuneID = tunes_copyrightholders.TuneID
         AND tunes_copyrightholders.CopyrightHolderID = 1
      LEFT JOIN wsf_shiny.songs
      ON psalmsongs.SongID = songs.SongID
         AND songs.Copyrighted = 'Y'
      LEFT JOIN (SELECT TuneID, MAX(SongID) AS SongID
                 FROM wsf.tunes_canonicalsongs
                 GROUP BY TuneID
                 HAVING COUNT(*) = 1) one_canonical_song
      ON tunes.TuneID = one_canonical_song.TuneID
      LEFT JOIN wsf.songs canonical_song
      ON one_canonical_song.SongID = canonical_song.SongID
 WHERE songs.SongID IS NULL
);
COMMIT;

-- Add more psalm song info
ALTER TABLE wsf_shiny.psalmsongs
ADD HTMLAlternatives blob;
SET SQL_SAFE_UPDATES = 0;
UPDATE wsf_shiny.psalmsongs
       LEFT JOIN (SELECT PsalmSongID,
                         GROUP_CONCAT(DISTINCT
                                      CONCAT('<div>',
                                             '<b>', TuneDisplayName, '</b>',
                                             CASE WHEN TuneEntries IS NOT NULL
                                                       THEN CONCAT('<br/><i>',
                                                                   TuneEntries,
                                                                   '</i>')
                                             END,
                                             CASE WHEN Notes IS NOT NULL
                                                       THEN CONCAT('<br/><p>',
                                                                   Notes,
                                                                   '</p>')
                                             END,
                                             '</div>')
                                      ORDER BY TuneDisplayName
                                      SEPARATOR '') AS AlternativeTunes
                  FROM wsf_shiny.psalmsongs_alternativetunes
                       LEFT JOIN (SELECT TuneID,
                                         GROUP_CONCAT(EntryString
                                                      ORDER BY SongbookName, EntryNumber
                                                      SEPARATOR ', ') AS TuneEntries
                                  FROM wsf.songinstances_tunes
                                       INNER JOIN wsf_shiny.songinstances_songbooks
                                       ON songinstances_tunes.SongInstanceID = songinstances_songbooks.SongInstanceID
                                  GROUP BY TuneID) tuneentries
                       ON psalmsongs_alternativetunes.TuneID = tuneentries.TuneID
                  GROUP BY PsalmSongID) psalmsongs_alternativetunes
       ON CONVERT(psalmsongs.PsalmSongID USING utf8mb4) = psalmsongs_alternativetunes.PsalmSongID
SET psalmsongs.HTMLAlternatives = CONCAT('<p></p>',
                                         psalmsongs_alternativetunes.AlternativeTunes);
SET SQL_SAFE_UPDATES = 1;

-- Table that connects tunes and canonical songs
DROP TABLE IF EXISTS wsf_shiny.tunes_canonicalsongs;
CREATE TABLE wsf_shiny.tunes_canonicalsongs AS
(SELECT TuneID, SongID
 FROM wsf.tunes_canonicalsongs);
COMMIT;