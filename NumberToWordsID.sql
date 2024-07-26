drop function NumberToWordsID
go

CREATE FUNCTION [dbo].[NumberToWordsID] (@Value DECIMAL(18,2)) 
RETURNS VARCHAR(1000)
AS
BEGIN
    DECLARE
        @Result VARCHAR(1000),
        @WholePart MONEY,
        @FractionalPart MONEY,
        @WordsWholePart CHAR(1024),
        @WordsFractionalPart CHAR(1024),
        @StringNumber VARCHAR(1024),
        @Index INT,
        @SubIndex INT,
        @Digit INT,
        @NumberLength INT,
        @LargeUnit VARCHAR(10),
        @SmallUnit VARCHAR(10),
        @TempString VARCHAR(255),
        @IsThreeDigitZero BIT,
        @PreviousDigit INT,
        @UnitWord VARCHAR(255);

    SELECT @WholePart = CAST(ROUND(@Value, 0, 1) AS MONEY);
    SELECT @FractionalPart = CAST(RIGHT(RTRIM(CAST(@Value AS VARCHAR(100))), 2) AS MONEY);
    SET @StringNumber = CONVERT(VARCHAR(25), @WholePart, 0);
    SET @StringNumber = LEFT(@StringNumber, LEN(@StringNumber) - 3);
    SET @NumberLength = LEN(@StringNumber);
    SET @WordsWholePart = '';
    SET @Index = 0;

    WHILE @Index < @NumberLength
    BEGIN
        SET @TempString = '';
        SELECT @LargeUnit = CASE @Index
                                WHEN 3 THEN ' RIBU'
                                WHEN 6 THEN ' JUTA'
                                WHEN 9 THEN ' MILIAR'
                                WHEN 12 THEN ' TRILIUN'
                                WHEN 15 THEN ' BILIUN'
                                ELSE ''
                            END;
        SET @IsThreeDigitZero = 1;
        SET @SubIndex = 0;

        WHILE (@SubIndex < 3 AND (@NumberLength - @Index - @SubIndex) > 0)
        BEGIN
            SET @Digit = CONVERT(INT, SUBSTRING(@StringNumber, @NumberLength - @Index - @SubIndex, 1));
            SELECT @SmallUnit = CASE @SubIndex
                                    WHEN 0 THEN ''
                                    WHEN 1 THEN ' PULUH'
                                    WHEN 2 THEN ' RATUS'
                                    WHEN 3 THEN ' RIBU'
                                END;
            IF @Digit <> 0
            BEGIN
                SET @IsThreeDigitZero = 0;
                SET @PreviousDigit = CONVERT(INT, SUBSTRING(@StringNumber, @NumberLength - @Index - @SubIndex + 1, 1));

                IF @Digit = 1
                BEGIN
                    IF @SubIndex = 0
                    BEGIN
                        IF @NumberLength - @Index - @SubIndex - 2 >= 0 OR @NumberLength = 1 OR @Index > 3
                            SET @TempString = ' SATU' + @TempString;
                        ELSE
                            SET @TempString = ' SE' + LTRIM(@SmallUnit + @TempString);
                    END;
                    IF @SubIndex = 1
                    BEGIN
                        IF @PreviousDigit = 0
                            SET @TempString = ' SEPULUH';
                        ELSE
                        BEGIN
                            IF @PreviousDigit = 1
                                SET @TempString = ' SEBELAS';
                            ELSE
                            BEGIN
                                SELECT @UnitWord = CASE @PreviousDigit
                                                        WHEN 0 THEN ''
                                                        WHEN 1 THEN ' SATU'
                                                        WHEN 2 THEN ' DUA'
                                                        WHEN 3 THEN ' TIGA'
                                                        WHEN 4 THEN ' EMPAT'
                                                        WHEN 5 THEN ' LIMA'
                                                        WHEN 6 THEN ' ENAM'
                                                        WHEN 7 THEN ' TUJUH'
                                                        WHEN 8 THEN ' DELAPAN'
                                                        WHEN 9 THEN ' SEMBILAN'
                                                    END;
                                SET @TempString = @UnitWord + ' BELAS';
                            END;
                        END;
                    END;
                    IF @SubIndex = 2
                        SET @TempString = ' SE' + LTRIM(@SmallUnit) + @TempString;
                END
                ELSE
                BEGIN
                    SELECT @UnitWord = CASE @Digit
                                            WHEN 0 THEN ''
                                            WHEN 1 THEN ' SATU'
                                            WHEN 2 THEN ' DUA'
                                            WHEN 3 THEN ' TIGA'
                                            WHEN 4 THEN ' EMPAT'
                                            WHEN 5 THEN ' LIMA'
                                            WHEN 6 THEN ' ENAM'
                                            WHEN 7 THEN ' TUJUH'
                                            WHEN 8 THEN ' DELAPAN'
                                            WHEN 9 THEN ' SEMBILAN'
                                        END;
                    SET @TempString = @UnitWord + @SmallUnit + @TempString;
                END;
            END;
            SET @SubIndex = @SubIndex + 1;
        END;

        IF @IsThreeDigitZero = 0
            SET @WordsWholePart = @TempString + @LargeUnit + @WordsWholePart;
        
        SET @Index = @Index + 3;
    END;

    SET @StringNumber = CONVERT(VARCHAR(25), @FractionalPart, 0);
    SET @StringNumber = LEFT(@StringNumber, LEN(@StringNumber) - 3);
    SET @NumberLength = LEN(@StringNumber);
    SET @WordsFractionalPart = '';
    SET @Index = 0;

    WHILE @Index < @NumberLength
    BEGIN
        SET @TempString = '';
        SELECT @LargeUnit = CASE @Index
                                WHEN 3 THEN ' RIBU'
                                WHEN 6 THEN ' JUTA'
                                WHEN 9 THEN ' MILIAR'
                                WHEN 12 THEN ' TRILIUN'
                                WHEN 15 THEN ' BILIUN'
                                ELSE ''
                            END;
        SET @IsThreeDigitZero = 1;
        SET @SubIndex = 0;

        WHILE (@SubIndex < 3 AND (@NumberLength - @Index - @SubIndex) > 0)
        BEGIN
            SET @Digit = CONVERT(INT, SUBSTRING(@StringNumber, @NumberLength - @Index - @SubIndex, 1));
            SELECT @SmallUnit = CASE @SubIndex
                                    WHEN 0 THEN ''
                                    WHEN 1 THEN ' PULUH'
                                    WHEN 2 THEN ' RATUS'
                                    WHEN 3 THEN ' RIBU'
                                END;
            IF @Digit <> 0
            BEGIN
                SET @IsThreeDigitZero = 0;
                SET @PreviousDigit = CONVERT(INT, SUBSTRING(@StringNumber, @NumberLength - @Index - @SubIndex + 1, 1));

                IF @Digit = 1
                BEGIN
                    IF @SubIndex = 0
                    BEGIN
                        IF @NumberLength - @Index - @SubIndex - 2 >= 0 OR @NumberLength = 1 OR @Index > 3
                            SET @TempString = ' SATU' + @TempString;
                        ELSE
                            SET @TempString = ' SE' + LTRIM(@SmallUnit + @TempString);
                    END;
                    IF @SubIndex = 1
                    BEGIN
                        IF @PreviousDigit = 0
                            SET @TempString = ' SEPULUH';
                        ELSE
                        BEGIN
                            IF @PreviousDigit = 1
                                SET @TempString = ' SEBELAS';
                            ELSE
                            BEGIN
                                SELECT @UnitWord = CASE @PreviousDigit
                                                        WHEN 0 THEN ''
                                                        WHEN 1 THEN ' SATU'
                                                        WHEN 2 THEN ' DUA'
                                                        WHEN 3 THEN ' TIGA'
                                                        WHEN 4 THEN ' EMPAT'
                                                        WHEN 5 THEN ' LIMA'
                                                        WHEN 6 THEN ' ENAM'
                                                        WHEN 7 THEN ' TUJUH'
                                                        WHEN 8 THEN ' DELAPAN'
                                                        WHEN 9 THEN ' SEMBILAN'
                                                    END;
                                SET @TempString = @UnitWord + ' BELAS';
                            END;
                        END;
                    END;
                    IF @SubIndex = 2
                        SET @TempString = ' SE' + LTRIM(@SmallUnit) + @TempString;
                END
                ELSE
                BEGIN
                    SELECT @UnitWord = CASE @Digit
                                            WHEN 0 THEN ''
                                            WHEN 1 THEN ' SATU'
                                            WHEN 2 THEN ' DUA'
                                            WHEN 3 THEN ' TIGA'
                                            WHEN 4 THEN ' EMPAT'
                                            WHEN 5 THEN ' LIMA'
                                            WHEN 6 THEN ' ENAM'
                                            WHEN 7 THEN ' TUJUH'
                                            WHEN 8 THEN ' DELAPAN'
                                            WHEN 9 THEN ' SEMBILAN'
                                        END;
                    SET @TempString = @UnitWord + @SmallUnit + @TempString;
                END;
            END;
            SET @SubIndex = @SubIndex + 1;
        END;

        IF @IsThreeDigitZero = 0
            SET @WordsFractionalPart = @TempString + @LargeUnit + @WordsFractionalPart;
        
        SET @Index = @Index + 3;
    END;

    IF @WordsFractionalPart <> ''
    BEGIN
        SET @WordsFractionalPart = LTRIM(RTRIM(@WordsFractionalPart)) + ' SEN';
    END;

    SELECT @Result = LTRIM(RTRIM(@WordsWholePart));
    SET @Result = REPLACE(@Result, 'SE RIBU', 'SERIBU');
    RETURN ISNULL(@Result, 0);
END
