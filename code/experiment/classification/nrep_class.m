%%
%   polygon_nrep_class.m
%
%   homa polygon-pattern NREP condition
%   with transfer classification phase
%   transfer phase presents the three prototypes, the 15 new low distortions,
%   15 new medium distortions, the 15 new high distortions and 15 old medium distortions
%   (from three categories)
%
try
    clear all;
    data_location=[pwd '\data\'];
    subid=input(' subject # ');
    filename=[data_location 'polynrep' num2str(subid) '.txt'];
    allvars=[data_location 'polynrep' num2str(subid)];
    if ~exist('data','dir')
        mkdir('data');
    end
    if ~exist(filename,'file')
        fid=fopen(filename,'wt');
        s=RandStream('mt19937ar','Seed','shuffle');
        RandStream.setGlobalStream(s);
        HideCursor;
        KbName('UnifyKeyNames');
        %
        % construct the training and transfer sets
        %
        ntrain=15;
        ntrans=63;
        %nblock=input(' number of pilot training blocks [15] ');
        %nblocktest=input(' number of pilot test blocks [1] ');
        nblocktrain=4; %nblocktrain=15;
        nblocktest=1;
        ntrial=ntrain;
        textsize=30;
        fixation_size=20;
        yoffset=100;
        yoffset2=200;
        xadjust=100;
        feedcorroffset=300;
        feedxnameoffset=100;
        feedynameoffset=100;
        
        %%%
        for cat=1:3
            prot=genDotPatterns(9,'prototype');
            for k=1:9
                for m=1:2
                    proto(cat,k,m)=prot(k,m);
                end
            end
            for ipat=1:(5*15)
                dot=genDotPatterns(9,'med',prot);
                for k=1:9
                    train(cat,ipat,k,1)=dot(k,1);
                    train(cat,ipat,k,2)=dot(k,2);
                end
            end
            for ipat=1:5
                dot=genDotPatterns(9,'low',prot);
                for k=1:9
                    newlow(cat,ipat,k,1)=dot(k,1);
                    newlow(cat,ipat,k,2)=dot(k,2);
                end
            end
            for ipat=1:5
                dot=genDotPatterns(9,'med',prot);
                for k=1:9
                    newmed(cat,ipat,k,1)=dot(k,1);
                    newmed(cat,ipat,k,2)=dot(k,2);
                end
            end
            for ipat=1:5
                dot=genDotPatterns(9,'high',prot);
                for k=1:9
                    newhigh(cat,ipat,k,1)=dot(k,1);
                    newhigh(cat,ipat,k,2)=dot(k,2);
                end
            end
        end
        
        
        
        %isi=input('isi (secs) ');
        %scale=input(' scale [10] ');
        %radius=input(' radius [5] ');
        isi=.5;
        scale=10;
        radius=5;
        %
        %  set up Screen and define screen-related constants
        %
        %[wind1 rect] = Screen('OpenWindow',0,[255 255 255],[50 50 1200 700]);
        Screen('Preference', 'SkipSyncTests', 1);
        [wind1,rect] = Screen('OpenWindow',0,[255 255 255]);
        
        centerx=(rect(3)-rect(1))/2;
        centery=(rect(4)-rect(2))/2;
        topscreen=rect(2)+50;
        bottomscreen=rect(4)-50;
        %
        fixation='*';
        press_space='When Ready, Press Space to Begin ';
        endblock='End of Test Block ';
        training_end='End of Training Phase';
        thanks='Thank You, the Experiment is Over!';
        pressq='(Press ''q'' to exit)';
        prompt_class='Category A, B, or C?';
        prompt_rec='Old or New?';
        text_correct='CORRECT!';
        text_incorrect='INCORRECT';
        text_okay='OKAY';
        percentage='Percent Correct= ';
        
        catname{1}='A';
        catname{2}='B';
        catname{3}='C';
        
        Screen('TextSize',wind1,textsize);
        textbounds_thanks=Screen('TextBounds',wind1,thanks);
        textbounds_pressq=Screen('TextBounds',wind1,pressq);
        textbounds_press_space=Screen('TextBounds',wind1,press_space);
        textbounds_endblock=Screen('TextBounds',wind1,endblock);
        textbounds_training_end=Screen('TextBounds',wind1,training_end);
        textbounds_prompt_class=Screen('TextBounds',wind1,prompt_class);
        textbounds_prompt_rec=Screen('TextBounds',wind1,prompt_rec);
        textbounds_correct=Screen('TextBounds',wind1,text_correct);
        textbounds_incorrect=Screen('TextBounds',wind1,text_incorrect);
        textbounds_okay=Screen('TextBounds',wind1,text_okay);
        textbounds_percentage=Screen('TextBounds',wind1,percentage);
        
        phase_store=[];
        block_store=[];
        trial_store=[];
        itemtype_store=[];
        token_store=[];
        resp_store=[];
        correct_store=[];
        cat_store=[];
        rt_store=[];
        
        legalkeys_class={'r','t','y'};
        legalkeys_rec={'f','j'};
        %
        %%
        WaitSecs(1)
        %%
        %  start of training phase of experiment
        %
        %   present instructions
        %
        instructions_poly_train(wind1,rect);
        %
        Screen('TextSize',wind1,textsize);
        Screen('DrawText',wind1,press_space,rect(3)/2-textbounds_press_space(3)/2,rect(4)/2-textbounds_press_space(4)/2);
        Screen('Flip',wind1);
        WaitSecs(.5);
        legal=0;
        while legal == 0
            [keydown,secs,keycode]=KbCheck;
            key=KbName(keycode);
            if strcmp(key,'space')
                legal=1;
            end
        end
        Screen('Flip',wind1);
        WaitSecs(1);
        %
        tot_trials=0;
        itemtype=1;
        phase=1;
        %
        %  nblock blocks of ntrial training trials
        %
        for block=1:nblocktrain
            order=randperm(ntrain);
            for trial=1:ntrial
                tot_trials=tot_trials+1;
                istim=order(trial);
                icat=fix((istim-1)/5)+1;
                token=istim-5*(icat-1)+5*(block-1);
                for k=1:9
                    image(k,1)=train(icat,token,k,1);
                    image(k,2)=train(icat,token,k,2);
                end
                %
                %  present dot pattern and collect response
                %
                for k=1:8
                    Screen('DrawLine',wind1,[0 0 0],centerx+scale*image(k,1),centery+scale*image(k,2), centerx+scale*image(k+1,1), centery+scale*image(k+1,2));
                end
                Screen('DrawLine',wind1,[0 0 0],centerx+scale*image(9,1),centery+scale*image(9,2), centerx+scale*image(1,1), centery+scale*image(1,2));
                Screen('DrawText',wind1,prompt_class,rect(3)/2-textbounds_prompt_class(3)/2,topscreen);
                Screen('Flip',wind1);
                legal=0;
                start=GetSecs;
                while legal == 0
                    [keydown,secs,keycode] = KbCheck;
                    key=KbName(keycode);
                    if ischar(key)
                        if any(strcmp(key,legalkeys_class))
                            legal=1;
                            rt=secs-start;
                        elseif strcmp(key,'q')
                            error('Debug quit!');
                        end
                    end
                end
                %
                % determine the subject's response
                %
                resp=0;
                switch key
                    case 'r'
                        resp=1;
                    case 't'
                        resp=2;
                    case 'y'
                        resp=3;
                end
                corr=0;
                if resp == icat
                    corr=1;
                end
                KbReleaseWait(); 
                Screen('Flip',wind1);
                
                %   present feedback while pattern remains on screen
                %
                actualname=catname{icat};
                for k=1:8
                    Screen('DrawLine',wind1,[0 0 0],centerx+scale*image(k,1), centery+scale*image(k,2), centerx+scale*image(k+1,1), centery+scale*image(k+1,2));
                end
                Screen('DrawLine',wind1,[0 0 0],centerx+scale*image(9,1),centery+scale*image(9,2), centerx+scale*image(1,1), centery+scale*image(1,2));
                if corr == 1
                    Screen('DrawText',wind1,text_correct,rect(3)/2-textbounds_correct(3)/2,centery+feedcorroffset);
                else
                    Screen('DrawText',wind1,text_incorrect,rect(3)/2-textbounds_incorrect(3)/2,centery+feedcorroffset);
                end
                Screen('DrawText',wind1,actualname,centerx,centery+feedcorroffset+feedynameoffset);
                Screen('Flip',wind1);
                if corr == 1
                    WaitSecs(1);
                elseif corr == 0
                    WaitSecs(1);
                end
                Screen('Flip',wind1);
                %%
                %               record results
                %
                tot_trials=tot_trials+1;
                phase_store(tot_trials)=phase;
                block_store(tot_trials)=block;
                trial_store(tot_trials)=trial;
                itemtype_store(tot_trials)=itemtype;
                resp_store(tot_trials)=resp;
                cat_store(tot_trials)=icat;
                rt_store(tot_trials)=rt;
                corr_store(tot_trials)=corr;
                token_store(tot_trials)=token;
                for k=1:9
                    coord_store(tot_trials,k,1)=image(k,1);
                    coord_store(tot_trials,k,2)=image(k,2);
                end
                %%
                %write to output text file
                %
                fprintf(fid,'%5d',phase_store(tot_trials),block_store(tot_trials),trial_store(tot_trials),itemtype_store(tot_trials),...
                    cat_store(tot_trials),token_store(tot_trials),...
                    resp_store(tot_trials),corr_store(tot_trials));
                fprintf(fid,'%10d',round(1000*rt_store(tot_trials)));
                for k=1:9
                    for m=1:2
                        fprintf(fid,'%5d',coord_store(tot_trials,k,m));
                    end
                end
                fprintf(fid,'\n');
                WaitSecs(isi);
            end   % trial
        end   %  block
        %
        Screen('DrawText',wind1,training_end,rect(3)/2-textbounds_training_end(3)/2,rect(4)/2-textbounds_training_end(4)/2);
        Screen('Flip',wind1);
        WaitSecs(2);
        %
        %  start test phase
        %
        %   present instructions
        %
        instructions_poly_test(wind1,rect);
        %
        Screen('TextSize',wind1,textsize);
        Screen('DrawText',wind1,press_space,rect(3)/2-textbounds_press_space(3)/2,rect(4)/2-textbounds_press_space(4)/2);
        Screen('Flip',wind1);
        WaitSecs(.5);
        legal=0;
        while legal == 0
            [keydown,secs,keycode]=KbCheck;
            key=KbName(keycode);
            if strcmp(key,'space')
                legal=1;
            end
        end
        Screen('Flip',wind1);
        WaitSecs(1);
        
        %
        %   nblocktest blocks of ntrans test trials
        %
        phase=2;
        for block=1:nblocktest
            blockcount=0;
            blockrand=randperm(15);
            percent_correct=0;
            tot_test=0;
            order=randperm(ntrans);
            for trial=1:ntrans
                tot_trials=tot_trials+1;
                tot_test=tot_test+1;
                istim=order(trial);
                %
                % compute itemtype and construct the dot-pattern image
                %
                if istim <= 15
                    itemtype=3;
                    icat=fix((istim-1)/5)+1;
                    token=istim-5*(icat-1);
                    for k=1:9
                        for m=1:2
                            image(k,m)=newlow(icat,token,k,m);
                        end
                    end
                elseif istim >= 16 && istim <=30
                    itemtype=4;
                    jstim=istim-15;
                    icat=fix((jstim-1)/5)+1;
                    token=jstim-5*(icat-1);
                    for k=1:9
                        for m=1:2
                            image(k,m)=newmed(icat,token,k,m);
                        end
                    end
                elseif istim >= 31 && istim <=45
                    itemtype=5;
                    jstim=istim-30;
                    icat=fix((jstim-1)/5)+1;
                    token=jstim-5*(icat-1);
                    for k=1:9
                        for m=1:2
                            image(k,m)=newhigh(icat,token,k,m);
                        end
                    end
                elseif istim >= 46 && istim<=60
                    itemtype=1;
                    jstim=istim-45;
                    icat=fix((jstim-1)/5)+1;
                    blockcount=blockcount+1;
                    token=jstim-5*(icat-1)+5*(blockrand(blockcount)-1);
                    for k=1:9
                        for m=1:2
                            image(k,m)=train(icat,token,k,m);
                        end
                    end
                elseif istim >= 61
                    itemtype=2;
                    icat=istim-60;
                    token=0;
                    for k=1:9
                        for m=1:2
                            image(k,m)=proto(icat,k,m);
                        end
                    end
                end
                %
                % present image and prompt and collect old-new response
                %
                for k=1:8
                    Screen('DrawLine',wind1,[0 0 0],centerx+scale*image(k,1), centery+scale*image(k,2), centerx+scale*image(k+1,1), centery+scale*image(k+1,2));
                end
                Screen('DrawLine',wind1,[0 0 0],centerx+scale*image(9,1),centery+scale*image(9,2), centerx+scale*image(1,1), centery+scale*image(1,2));
                Screen('DrawText',wind1,prompt_class,rect(3)/2-textbounds_prompt_class(3)/2,topscreen);
                Screen('Flip',wind1);
                legal=0;
                start=GetSecs;
                while legal == 0
                    [keydown,secs,keycode] = KbCheck;
                    key=KbName(keycode);
                    if ischar(key)
                        if any(strcmp(key,legalkeys_class))
                            rt=secs-start;
                            legal=1;
                        elseif strcmp(key,'q')
                            error('Debug quit!');
                        end
                    end
                end
                %%
                %
                % determine the subject's response and whether it is correct
                %
                resp=0;
                switch key
                    case 'r'
                        resp=1;
                    case 't'
                        resp=2;
                    case 'y'
                        resp=3;
                end
                corr=0;
                if resp == icat
                    corr=1;
                end
                KbReleaseWait(); 
                Screen('Flip',wind1);
                if corr == 1
                    percent_correct=percent_correct+1;
                end
                
                
                %
                %  let subject know the response was recorded
                %
                
                Screen('DrawText',wind1,text_okay,rect(3)/2-textbounds_okay(3)/2,centery+feedcorroffset);
                Screen('Flip',wind1);
                WaitSecs(1);
                Screen('Flip',wind1);
                %%
                %               record results
                %
                tot_trials=tot_trials+1;
                phase_store(tot_trials)=phase;
                block_store(tot_trials)=block;
                trial_store(tot_trials)=trial;
                itemtype_store(tot_trials)=itemtype;
                resp_store(tot_trials)=resp;
                cat_store(tot_trials)=icat;
                rt_store(tot_trials)=rt;
                corr_store(tot_trials)=corr;
                token_store(tot_trials)=token;
                for k=1:9
                    coord_store(tot_trials,k,1)=image(k,1);
                    coord_store(tot_trials,k,2)=image(k,2);
                end
                %%
                %               write to output text file
                %
                fprintf(fid,'%5d',phase_store(tot_trials),block_store(tot_trials),trial_store(tot_trials),itemtype_store(tot_trials),...
                    cat_store(tot_trials),token_store(tot_trials),...
                    resp_store(tot_trials),corr_store(tot_trials));
                fprintf(fid,'%10d',round(1000*rt_store(tot_trials)));
                for k=1:9
                    for m=1:2
                        fprintf(fid,'%5d',coord_store(tot_trials,k,m));
                    end
                end
                fprintf(fid,'\n');
                WaitSecs(isi);
            end   % trial
            percent_correct=percent_correct/tot_test;
            pcvalue=round(100*percent_correct);
            Screen('DrawText',wind1,[endblock num2str(block)],rect(3)/2-textbounds_endblock(3)/2,rect(4)/2-textbounds_endblock(4)/2);
            pcvalue_report=[percentage num2str(pcvalue)];
            Screen('DrawText',wind1,pcvalue_report,rect(3)/2-textbounds_percentage(3)/2,rect(4)/2-textbounds_percentage(4)/2+200);
            Screen('Flip',wind1);
            WaitSecs(3);
            Screen('Flip',wind1);
            WaitSecs(1);
        end   %  block
        %
        fclose(fid);
        save(allvars);
        Screen('DrawText',wind1,thanks,rect(3)/2-textbounds_thanks(3)/2,rect(4)/2-textbounds_thanks(4)/2);
        %    Screen('DrawText',wind1,pressq,rect(3)/2-textbounds_pressq(3)/2,rect(4)/2-textbounds_pressq(4)/2+50);
        Screen('Flip',wind1);
        WaitSecs(5);
        Screen('Flip',wind1);
        sca;
    else
        disp('Error: the file already exists!')
    end
catch
    sca;
    Priority(0);
    ShowCursor();
    psychrethrow(psychlasterror);
end

